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
    , getMove
    , teamName) where

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

type MazePos = (Int, Int)
type MazeMove = (Int, Int)

data MazeItem = Wall | Free deriving (Eq)
instance Show MazeItem where
    show Wall = "#"
    show Free = " "

data Maze = Maze (A.Array MazePos MazeItem) deriving (Eq)
rowIdx :: Maze -> [Int]
rowIdx (Maze a) = [(snd . fst $ A.bounds a) .. (snd . snd $ A.bounds a)]
colIdx :: Maze -> [Int]
colIdx (Maze a) = [(fst . fst $ A.bounds a) .. (fst . snd $ A.bounds a)]

mazeItem :: Maze -> MazePos -> MazeItem
mazeItem (Maze a) (i, j) = a A.! (i, j)

instance FromJSON MazeItem where
    parseJSON (Bool True) = return Wall
    parseJSON (Bool False) = return Free
    parseJSON _ = mzero

instance FromJSON Maze where
    parseJSON (Object o) = do
        (Array mazeData) <- o .: "data"
        elements <- mapM parseJSON $ Vector.toList mazeData
        width <- (o .: "width")
        height <- (o .: "height")
        let dimensions = ((0, 0), (width - 1, height - 1)) :: (MazePos, MazePos)
        let elemIdx = do
              j <- [0 .. (height - 1)]
              i <- [0 .. (width - 1)]
              return (i, j)
        let ar = A.array dimensions (zip elemIdx elements)
        return $ Maze ar

data FoodList = FoodList (Vector.Vector MazePos)

instance FromJSON FoodList where
    parseJSON (Array foodList) = do
        food <- Vector.mapM parsePos foodList
        return $ FoodList food
      where
        parsePos jsn = do
            [x, y] <- parseJSON jsn
            return (x, y)

data Universe = Universe Maze FoodList -- deriving (Eq)
data GameState = GameState Object deriving (Eq, Show)

data PelitaData = TeamNameData
                | SetInitialData Universe GameState
                | GetMoveData Universe GameState
                | ExitPelita deriving (Show)

instance FromJSON Universe where
    parseJSON (Object o) = do
        let maze = o .: "maze"
        let foodList = o .: "food"
        Universe <$> maze <*> foodList -- <$> o .: "width" <*> o .: "height"
    parseJSON _ = undefined

instance Show Universe where
    show (Universe maze (FoodList foodList)) = join $ do
        row <- rowIdx maze
        return $ "\n" ++ showRow row
      where
        showRow row = join $ do
            col <- colIdx maze
            return $ (mazeItemOrFood maze (col, row))
        mazeItemOrFood maze pos = if pos `Vector.elem` foodList
                                      then "."
                                      else show $ mazeItem maze pos

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
        parseData "team_name" (Object o) = return TeamNameData
        parseData "set_initial" (Object o) = SetInitialData
                                           <$> o .: "universe"
                                           <*> o .: "game_state"
        parseData "get_move" (Object o) = GetMoveData
                                        <$> o .: "universe"
                                        <*> o .: "game_state"
        parseData "exit" _ = return ExitPelita
        parseData _ _ = undefined

class Player p where
    teamName :: State p String
    setInitial :: Universe -> GameState -> State p ()
    getMove :: Universe -> GameState -> State p MazeMove


action :: Player pl => PelitaData -> State pl PlayerResult
action (TeamNameData) =
    modifyResult <$> teamName
    where
        modifyResult res = (PlayerResult (toJSON res) True)

action (SetInitialData universe gameState) =
    modifyResult <$> runPlayer
    where
        runPlayer = setInitial universe gameState >> teamName
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


data PlayerResult = PlayerResult Value Bool

withPelita :: Player pl => StateT pl IO ()
withPelita = do
    args <- liftIO getArgs
    progName <- liftIO getProgName
    when (length args < 1) . liftIO $ do
        hPutStrLn stderr $ "usage: " ++ progName ++ " <server-address>"
        exitFailure

    player <- get
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
