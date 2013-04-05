import Control.Monad
import Control.Applicative

import System.Exit
import System.IO
import System.Environment
import System.ZMQ

import qualified Data.Map as Map
import Data.ByteString.Char8 (pack, unpack)

import Text.JSON
import Text.JSON.Types
import Text.JSON.String (readJSValue, showJSValue, runGetJSON)

processJson :: JSValue -> Either String JSValue
processJson = \a -> Right a


newtype PelitaMsg = PelitaMsg (String, String, JSValue) deriving (Show)

extractPelitaMsg :: JSValue -> Either String PelitaMsg
extractPelitaMsg (JSObject (JSONObject listObj)) = 
  eitheredMsg where
    mappedObj = Map.fromList listObj
    -- extract the values from the Map
    maybeMsg :: Maybe PelitaMsg
    maybeMsg = do
      JSString (JSONString action) <- Map.lookup "__action__" mappedObj
      JSString (JSONString uuid) <- Map.lookup "__uuid__" mappedObj
      theData <- Map.lookup "__data__" mappedObj
      return $ PelitaMsg (action, uuid, theData)

    eitheredMsg = case maybeMsg of
      Just x -> Right x
      Nothing -> Left "could not match this JSON"
extractPelitaMsg other = Left "could not match this JSON"


doWithTeamName :: JSValue
doWithTeamName = showJSON "Haskell Stopping Player"

doWithAction :: String -> JSValue -> JSValue
doWithAction "team_name" _ = doWithTeamName
doWithAction other _ = showJSON([("move", [0, 0])] :: [(String, [Int])])

doWithPelitaMsg :: PelitaMsg -> JSValue
doWithPelitaMsg (PelitaMsg (action, uuid, theData)) = JSObject $ toJSObject [("__uuid__", JSString (JSONString uuid)),
                                                                             ("__return__", doWithAction action theData)]

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
        let strMessage = unpack message

        case runGetJSON readJSValue strMessage >>= extractPelitaMsg of
          Left  e -> error e
          Right j -> send server (pack (showJSValue (doWithPelitaMsg j) "")) [] -- putStrLn $ show j

--        putStrLn $ unwords ["Received request:", unpack message]

  return ()

