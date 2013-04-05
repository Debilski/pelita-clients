import Control.Monad
import Control.Applicative

import System.Exit
import System.IO
import System.Environment
import System.ZMQ

import Data.ByteString.Char8 (pack, unpack)

import Text.JSON
import Text.JSON.Types

processJson :: JSValue -> Either String JSValue
processJson = \a -> Right a


newtype PelitaMsg = PelitaMsg (String, String, JSValue) deriving (Show)

extractPelitaMsg :: JSValue -> Either String PelitaMsg
extractPelitaMsg (JSObject (dataObj)) = 
  eitheredMsg where
    -- extract the values from the dataObj
    resultMsg :: Result PelitaMsg
    resultMsg = do
      JSString (JSONString action) <- valFromObj "__action__" dataObj
      JSString (JSONString uuid) <- valFromObj "__uuid__" dataObj
      theData <- valFromObj "__data__" dataObj
      return $ PelitaMsg (action, uuid, theData)

    eitheredMsg = case resultMsg of
      Ok x -> Right x
      Error s -> Left ("could not match this JSON (" ++ s ++ ")")
extractPelitaMsg other = Left "could not match this JSON"


doWithTeamName :: JSValue
doWithTeamName = showJSON "Haskell Stopping Player"

doWithAction :: String -> JSValue -> JSValue
doWithAction "team_name" _ = doWithTeamName
doWithAction other _ = showJSON . toJSObject $ ([("move", [0, 0])] :: [(String, [Int])])

doWithPelitaMsg :: PelitaMsg -> JSValue
doWithPelitaMsg (PelitaMsg (action, uuid, theData)) = showJSON $ toJSObject [("__uuid__", showJSON uuid),
                                                                             ("__return__", doWithAction action theData)]
trace :: String -> IO String
trace s = putStrLn s >> return s

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

        case resultToEither (decode strMessage) >>= extractPelitaMsg of
          Left  e -> error e
          Right j -> trace(encode $ doWithPelitaMsg j) >>= \s -> send server (pack s) [] -- putStrLn $ show j

--        putStrLn $ unwords ["Received request:", unpack message]

  return ()

