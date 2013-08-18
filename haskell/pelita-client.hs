import Control.Monad
import Control.Applicative

import System.Exit
import System.IO
import System.Environment
import System.ZMQ

import Data.ByteString.Char8 (pack, unpack)

import Text.JSON
import Text.JSON.Types

-- newtype PelitaMsg = PelitaMsg (String, String, JSValue) deriving (Show)

newtype PelitaData = PelitaData (String) deriving (Show)

data PelitaMsg = PelitaMsg {
   action :: String
  ,uuid :: String
  ,theData :: PelitaData
} deriving (Show)

instance JSON PelitaMsg where
  readJSON (JSObject o) = return $ PelitaMsg { action=action, uuid=uuid, theData=(PelitaData theData) }
    where action   = grab o "__action__"
          uuid     = grab o "__uuid__"
          theData  = grab o "__data__"

          grab o s = case get_field o s of
                Nothing            -> error "Invalid field " ++ show s
                Just (JSString s') -> fromJSString s'

extractPelitaMsg :: JSValue -> Result PelitaMsg
extractPelitaMsg dataObj = readJSON dataObj

doWithTeamName :: JSValue
doWithTeamName = showJSON "Haskell Stopping Player"

doWithAction :: String -> PelitaData -> JSValue
doWithAction "team_name" _ = doWithTeamName
doWithAction other _ = showJSON . toJSObject $ ([("move", [0, 0])] :: [(String, [Int])])

doWithPelitaMsg :: PelitaMsg -> JSValue
doWithPelitaMsg (PelitaMsg action uuid theData) = showJSON $ toJSObject [("__uuid__", showJSON uuid),
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

        case (decode strMessage) >>= readJSON of
          Error e -> error e
          Ok j -> trace(encode $ doWithPelitaMsg j) >>= \s -> send server (pack s) [] -- putStrLn $ show j

--        putStrLn $ unwords ["Received request:", unpack message]

  return ()

