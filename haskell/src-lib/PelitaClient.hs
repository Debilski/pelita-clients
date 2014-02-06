{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PelitaClient (Player, withPelita, Universe(..), Maze(..), GameState, setInitial, getMove) where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Monoid
import Data.Traversable

import System.Exit
import System.IO
import System.Environment
import System.ZMQ4.Monadic

import qualified  Data.ByteString.Lazy  as B
import            Data.ByteString       (ByteString)

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types

import Data.HashMap.Strict
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector

import Data.Maybe (maybeToList)

import Debug.Trace

type MazeItems = Set.Set MazeItem
data MazeItem = Wall | Food deriving (Show, Eq, Ord)
data Maze = Maze { fromSeq :: Seq.Seq MazeItems,
                   mazeWidth :: Int,
                   mazeHeight :: Int } deriving (Show, Eq)

mazeItems :: Maze -> (Int, Int) -> MazeItems
mazeItems maze (i, j) = Seq.index (fromSeq maze) (i + j * (mazeWidth maze))

convertMazeString :: String -> MazeItems
convertMazeString str = Set.fromList $ str >>= (maybeToList . convertMazeChar)

convertMazeChar :: Char -> Maybe MazeItem
convertMazeChar '#' = Just Wall
convertMazeChar '.' = Just Food
convertMazeChar _ = Nothing

instance FromJSON Maze where
  parseJSON (Object o) = do
    (Array mazeData) <- o .: "data"
    Maze <$> (return $ fmap convertMazeString (Seq.fromList . Vector.toList $ fmap show mazeData)) <*> (o .: "width") <*> (o .: "height")


data Universe = Universe Maze deriving (Eq)
data GameState = GameState Object deriving (Eq, Show)

data PelitaData = GetTeamNameData
                | SetInitialData Universe GameState
                | GetMoveData Universe GameState
                | ExitPelita deriving (Eq, Show)

data PelitaMsg = PelitaMsg {
   action :: String
  ,uuid :: String
  ,theData :: PelitaData
} deriving (Show)

instance FromJSON Universe where
  parseJSON (Object o) = do
    (Object value) <- o .: "__value__"
    let maze = value .: "maze"

    Universe <$> maze -- <$> o .: "width" <*> o .: "height"
  parseJSON _ = undefined

instance Show Universe where
  show (Universe (Maze md mazeWidth mazeHeight)) = join $ do
    j <- [0 .. (mazeHeight - 1)]
    i <- [0 .. (mazeWidth - 1)]
    let items = Seq.index md (j * mazeWidth + i)
    return $ (if i == 0 then "\n" else "" ) ++ if Set.member Wall items
        then "#"
        else if Set.member Food items
           then "."
           else " "


instance FromJSON GameState where
  parseJSON (Object o) = return $ GameState Data.HashMap.Strict.empty -- GameState o
  parseJSON _ = undefined

instance FromJSON PelitaMsg where
  parseJSON (Object o) = PelitaMsg <$> action <*> uuid <*> data_
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

doWithTeamName :: Value
doWithTeamName =  toJSON ("Haskell Stopping Player" :: String)

doWithSetInitial :: Universe -> GameState -> Value
doWithSetInitial uni gs = toJSON $ (fromList [] :: HashMap String [Int])

doWithGetMove :: Universe -> GameState -> Value
doWithGetMove uni gs = trace (show uni) $ toJSON $ (fromList [("move", [0, 0])] :: HashMap String [Int])

doWithAction :: PelitaData -> Value
doWithAction (GetTeamNameData) = doWithTeamName
doWithAction (SetInitialData universe gameState) = doWithSetInitial universe gameState
doWithAction (GetMoveData universe gameState) = doWithGetMove universe gameState
doWithAction (ExitPelita) = toJSON $ (fromList [] :: HashMap String [Int])

class Player p where
  setInitial :: Universe -> GameState -> State p ()
  getMove :: Universe -> GameState -> State p (Int, Int)

data PlayerResult = PlayerResult Value Bool

withPelita :: Player pl => String -> pl -> IO ()
withPelita teamName p = do
  args <- getArgs
  progName <- getProgName
  when (length args < 1) $ do
    hPutStrLn stderr $ "usage: " ++ progName ++ " <server-address>"
    exitFailure

  runZMQ $ do
    -- Pair socket to talk to server
    sock <- socket Pair
    connect sock (args !! 0)

    playRound sock p
    return ()
      where

      playRound :: forall z a pl. (Sender a, Receiver a, Player pl) => Socket z a -> pl -> ZMQ z pl
      playRound sock p = do
        strMessage <- getMessage

        case eitherDecodeLazy strMessage of
          Left e -> error e
          Right incomingMessage -> nextAction -- sendValue sock (fst val) >> (snd val)
            where nextAction :: ZMQ z pl
                  nextAction =
                    let (PelitaMsg _ uuid pelitaData) = incomingMessage
                        (PlayerResult jsonValue finished, newPlayer) = runState (action pelitaData) p
                    in sendValue (wrapValue uuid jsonValue) >> if finished then (return newPlayer)
                                                                           else (playNext newPlayer)

                  action :: PelitaData -> State pl PlayerResult
                  action (GetTeamNameData) =
                    let jsonValue = toJSON teamName
                    in return $ PlayerResult jsonValue False

                  action (SetInitialData universe gameState) =
                    modifyResult $ runPlayer
                    where
                      playerState = setInitial universe gameState
                      runPlayer :: State pl ()
                      runPlayer = playerState
                      jsonValue :: State pl () -> State pl Value
                      jsonValue = fmap toJSON
                      wrapIt :: State pl Value -> State pl PlayerResult
                      wrapIt = fmap (\res -> (PlayerResult res False))
                      modifyResult :: State pl () -> State pl PlayerResult
                      modifyResult = wrapIt . jsonValue

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

                  wrapValue :: String -> Value -> Value
                  wrapValue uuid value = toJSON dict
                    where
                      dict :: HashMap String Value
                      dict = (fromList [("__uuid__" , toJSON uuid),
                                        ("__return__", value)])

            where
                  playNext :: pl -> ZMQ z pl
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


