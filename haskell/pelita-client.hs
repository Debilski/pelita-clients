{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Applicative
import Data.Monoid

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

import Debug.Trace

data Universe = Universe Int Int deriving (Eq, Show)
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
    return $ Universe w h -- <$> o .: "width" <*> o .: "height"
  parseJSON _ = undefined

instance FromJSON GameState where
  parseJSON (Object o) = return $ GameState Data.HashMap.Strict.empty -- GameState o
  parseJSON _ = undefined

instance FromJSON PelitaMsg where
  parseJSON (Object o) = trace (show o) $ PelitaMsg <$> action <*> uuid <*> data_
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

doWithPelitaMsg :: PelitaMsg -> Value
doWithPelitaMsg (PelitaMsg action uuid theData) = toJSON $ (fromList [("__uuid__" , toJSON uuid),
                                                                      ("__return__", doWithAction theData)] :: HashMap String Value)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  when (length args < 1) $ do
    hPutStrLn stderr $ "usage: " ++ progName ++ " <server-address>"
    exitFailure

  withContext 1 $ \context -> do  
    -- Socket to talk to server
    withSocket context Pair $ \server -> do
      connect server (args !! 0)

      forever $ do
        message <- receive server []
        let strMessage = B.fromChunks [message]

        case eitherDecode strMessage of
          Left e -> error e
          Right j -> send server (mconcat $ B.toChunks v) [] -- putStrLn $ show j
            where s = trace ("sending back " ++ (show v)) $ v
                  v = encode $ doWithPelitaMsg (trace (show j) j)

--        putStrLn $ unwords ["Received request:", unpack message]

  return ()

