{-# LANGUAGE OverloadedStrings #-}

module PelitaClient (Player, withPelita, Universe, GameState, setInitial, getMove) where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Monoid
import Data.Traversable

import System.Exit
import System.IO
import System.Environment
import System.ZMQ

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy as B

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

data MazeItem = Wall | Food deriving (Show, Eq, Ord)
data Maze = Maze { fromSeq :: Seq.Seq (Set.Set MazeItem) } deriving (Show, Eq)

convertMazeString :: String -> Set.Set MazeItem
convertMazeString str = Set.fromList $ str >>= (maybeToList . convertMazeChar)

convertMazeChar :: Char -> Maybe MazeItem
convertMazeChar '#' = Just Wall
convertMazeChar '.' = Just Food
convertMazeChar _ = Nothing

instance FromJSON Maze where
  parseJSON (Array a) = return $ Maze $ fmap convertMazeString (Seq.fromList . Vector.toList $ fmap show a)

data Universe = Universe Int Int Maze deriving (Eq)
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
    (Object maze) <- value .: "maze"
    w <- maze .: "width"
    h <- maze .: "height"
    mazeObject <- maze .: "data"
    return $ Universe w h mazeObject -- <$> o .: "width" <*> o .: "height"
  parseJSON _ = undefined

instance Show Universe where
  show (Universe w h (Maze m)) = join $ do

    j <- [0 .. (h - 1)]
    i <- [0 .. (w - 1)]
    let items = Seq.index m (j * w + i)
    return $ (if i == 0 then "\n" else "" ) ++ if Set.member Wall items then "#"
    else if Set.member Food items then "."
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

wrapValue uuid value = toJSON dict
  where
    dict :: HashMap String Value
    dict = (fromList [("__uuid__" , toJSON uuid),
                      ("__return__", value)])

doWithPelitaMsg :: (PelitaData -> (Value -> Value) -> IO pl) -> PelitaMsg -> IO pl
doWithPelitaMsg action (PelitaMsg actionStr uuid theData) = action theData (wrapValue uuid)

class Player p where
  setInitial :: Universe -> GameState -> State p ()
  getMove :: Universe -> GameState -> State p (Int, Int)

withPelita :: Player pl => String -> pl -> IO ()
withPelita teamName p = do
  args <- getArgs
  progName <- getProgName
  when (length args < 1) $ do
    hPutStrLn stderr $ "usage: " ++ progName ++ " <server-address>"
    exitFailure

  withContext 1 $ \context -> do
    -- Socket to talk to server
    withSocket context Pair $ \server -> do
      connect server (args !! 0)

      playRound server p
      return ()
        where
        getMessage :: Socket a -> IO B.ByteString
        getMessage server = fmap (\m -> B.fromChunks [m]) (receive server [])

        sendMessage :: Socket a -> B.ByteString -> IO ()
        sendMessage server str = trace (show str) $ send server (mconcat $ B.toChunks str) []

        sendValue :: Socket a -> Value -> IO ()
        sendValue server = sendMessage server . encode

        playRound :: Player pl => Socket a -> pl -> IO pl
        playRound server p = do
          strMessage <- getMessage server

          case eitherDecode strMessage of
            Left e -> error e
            Right msg -> val -- sendValue server (fst val) >> (snd val)
              where val = doWithPelitaMsg action msg

                    action (GetTeamNameData) wrapper = (sendValue server (wrapper $ toJSON teamName)) >> playRound server p

                    action (SetInitialData universe gameState) wrapper =
                      let (res, newP) = runState (setInitial universe gameState) p
                          jsonValue = toJSON res
                      in (sendValue server (wrapper jsonValue)) >> playRound server newP

                    action (GetMoveData universe gameState) wrapper =
                      let (moveTpl, newP) = runState (getMove universe gameState) p
                          move = [fst moveTpl, snd moveTpl] :: [Int]
                          jsonValue = toJSON $ fromList [("move" :: String, move)]
                          nextIO = playRound server newP
                      in sendValue server (wrapper jsonValue) >> nextIO


                    action (ExitPelita) wrapper =
                      let jsonValue = toJSON $ (fromList [] :: HashMap String [Int])
                          nextIO = return p
                      in sendValue server (wrapper jsonValue) >> nextIO

