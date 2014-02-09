{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PelitaClient
    (
      Player
    , withPelita
    , Universe(..)
    , Maze(..)
    , GameState
    , setInitial
    , getMove) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State

import           System.Environment
import           System.Exit
import           System.IO
import           System.ZMQ4.Monadic

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as B

import           Data.Aeson
import           Data.Aeson.Types

import           Data.HashMap.Strict
import qualified Data.Set             as Set
import qualified Data.Vector          as Vector

import qualified Data.Array           as A

import           Data.Maybe           (maybeToList)

import           Debug.Trace

type MazeItems = Set.Set MazeItem
data MazeItem = Wall | Food deriving (Eq, Ord)
instance Show MazeItem where
    show Wall = "#"
    show Food = "."

data Maze = Maze (A.Array (Int, Int) MazeItems) deriving (Eq)
rowIdx :: Maze -> [Int]
rowIdx (Maze a) = [(snd . fst $ A.bounds a) .. (snd . snd $ A.bounds a)]
colIdx :: Maze -> [Int]
colIdx (Maze a) = [(fst . fst $ A.bounds a) .. (fst . snd $ A.bounds a)]

mazeItems :: Maze -> (Int, Int) -> MazeItems
mazeItems (Maze a) (i, j) = a A.! (i, j)

convertMazeString :: String -> MazeItems
convertMazeString str = Set.fromList $ str >>= (maybeToList . convertMazeChar)

convertMazeChar :: Char -> Maybe MazeItem
convertMazeChar '#' = Just Wall
convertMazeChar '.' = Just Food
convertMazeChar _ = Nothing

instance FromJSON Maze where
    parseJSON (Object o) = do
        (Array mazeData) <- o .: "data"
        width <- (o .: "width")
        height <- (o .: "height")
        let dimensions = ((1, 1), (width, height)) :: ((Int, Int), (Int, Int))
        let elements = fmap convertMazeString (Vector.toList $ fmap show mazeData) :: [MazeItems]
        let elemIdx = do
            j <- [1 .. height]
            i <- [1 .. width]
            return (i, j)
        let ar = A.array dimensions (zip elemIdx elements)
        return $ Maze ar

-- (return $ fmap convertMazeString (Seq.fromList . Vector.toList $ fmap show mazeData))


data Universe = Universe Maze deriving (Eq)
data GameState = GameState Object deriving (Eq, Show)

data PelitaData = GetTeamNameData
                | SetInitialData Universe GameState
                | GetMoveData Universe GameState
                | ExitPelita deriving (Eq, Show)

instance FromJSON Universe where
    parseJSON (Object o) = do
        (Object value) <- o .: "__value__"
        let maze = value .: "maze"
        Universe <$> maze -- <$> o .: "width" <*> o .: "height"
    parseJSON _ = undefined

instance Show Universe where
    show (Universe maze) = join $ do
        row <- rowIdx maze
        return $ "\n" ++ showRow row
      where
        showRow row = join $ do
            col <- colIdx maze
            return $ characterFor (mazeItems maze (col, row))
        characterFor items = if Set.member Wall items
                                 then "#"
                                 else if Set.member Food items
                                      then "."
                                      else " "

instance FromJSON GameState where
    parseJSON (Object _) = return $ GameState Data.HashMap.Strict.empty -- GameState o
    parseJSON _ = undefined

type UUID = String
data PelitaMessage = PelitaMessage UUID PelitaData deriving (Show)

instance FromJSON PelitaMessage where
    parseJSON (Object o) = PelitaMessage <$> uuid <*> data_
      where
        action = o .: "__action__"
        uuid = o .: "__uuid__"
        data_ = join $ parseData <$> action <*> ((o .: "__data__") :: Parser Value)

        parseData :: String -> Value -> Parser PelitaData
        parseData "team_name" _ = return GetTeamNameData
        parseData "set_initial" (Object o) = SetInitialData
                                           <$> o .: "universe"
                                           <*> o .: "game_state"
        parseData "get_move" (Object o) = GetMoveData
                                        <$> o .: "universe"
                                        <*> o .: "game_state"
        parseData "exit" _ = return ExitPelita
        parseData _ _ = undefined

class Player p where
    setInitial :: Universe -> GameState -> State p ()
    getMove :: Universe -> GameState -> State p (Int, Int)

data PlayerResult = PlayerResult Value Bool

withPelita :: Player pl => String -> pl -> IO ()
withPelita teamName player = do
    args <- getArgs
    progName <- getProgName
    when (length args < 1) $ do
        hPutStrLn stderr $ "usage: " ++ progName ++ " <server-address>"
        exitFailure

    runZMQ $ do
        -- Pair socket to talk to server
        sock <- socket Pair
        connect sock (args !! 0)

        evalStateT (playRound sock) player
      where

        playRound :: forall z a pl. (Sender a, Receiver a, Player pl) => Socket z a -> StateT pl (ZMQ z) ()
        playRound sock = do
            strMessage <- lift getMessage

            case eitherDecodeLazy strMessage of
                Left e -> error e
                Right incomingMessage -> nextAction
                  where
                    nextAction :: StateT pl (ZMQ z) ()
                    nextAction = hoist (action pelitaData) >>= sendResult >>= checkContinue
                      where
                        hoist :: State pl PlayerResult -> StateT pl (ZMQ z) PlayerResult
                        hoist = StateT . (return .) . runState

                        sendResult (PlayerResult jsonValue finished) = do
                            lift $ sendValue (wrapValue uuid jsonValue)
                            return finished
                        checkContinue finished = if finished then return ()
                                                             else playNext
                        PelitaMessage uuid pelitaData = incomingMessage

                    action :: PelitaData -> State pl PlayerResult
                    action (GetTeamNameData) =
                        let jsonValue = toJSON teamName
                        in return $ PlayerResult jsonValue False

                    action (SetInitialData universe gameState) =
                        modifyResult <$> runPlayer
                        where
                          runPlayer = setInitial universe gameState
                          modifyResult res = (PlayerResult (toJSON res) False)

                    action (GetMoveData universe gameState) =
                        modifyResult <$> runPlayer
                        where
                          runPlayer = getMove universe gameState
                          modifyResult moveTpl =
                            let move = [fst moveTpl, snd moveTpl] :: [Int]
                                jsonValue = toJSON $ fromList [("move" :: String, move)]
                            in PlayerResult jsonValue False

                    action (ExitPelita) =
                        let jsonValue = toJSON $ (fromList [] :: HashMap String [Int])
                        in return $ PlayerResult jsonValue True

                    wrapValue :: UUID -> Value -> Value
                    wrapValue uuid value = toJSON dict
                      where
                        dict :: HashMap String Value
                        dict = (fromList [("__uuid__" , toJSON uuid),
                                          ("__return__", value)])

          where
            playNext :: StateT pl (ZMQ z) ()
            playNext = playRound sock

            getMessage :: Receiver a => ZMQ z ByteString
            getMessage = receive sock

            sendValue :: Value -> ZMQ z ()
            sendValue = sendMessage . encodeStrict

            sendMessage :: Sender a => ByteString -> ZMQ z ()
            sendMessage str = trace (show str) $ send sock [] str

        encodeStrict :: ToJSON a => a -> ByteString
        encodeStrict = B.toStrict . encode

        eitherDecodeLazy :: FromJSON a => ByteString -> Either String a
        eitherDecodeLazy = eitherDecode . B.fromStrict


