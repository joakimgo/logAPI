{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (when)
import           Data.String (fromString)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory (getModificationTime)
import           System.Posix.Files (fileSize, getFileStatus, FileStatus (..))
import           System.Environment (getArgs)
import           Data.Time.Clock (getCurrentTime, addUTCTime, NominalDiffTime(..))
import           Data.Maybe (catMaybes)
import           Servant.HTML.Lucid (HTML)
import           Lucid
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.Combinators (sourceDirectoryDeep)

-- configuration parameters
data Conf = Conf { path :: FilePath
                  ,lastdays :: Integer
                 } deriving (Show)

type FileName = Text
type FileSize = Integer

-- file representation
data LogFile = LogFile { filename :: FileName
                        ,filestatus :: FileStatus
                       }

-- api
type LogFileApi =
  -- /file?name=<filename>
  "file" :> QueryParam "name" Text :> Get '[HTML] (Html ()) :<|>
  -- /tail?lines=<lines>&name=<filename>
  "tail" :> QueryParam "lines" Int :> QueryParam "name" Text :> Get '[HTML] (Html ()) :<|>
   -- /list
  "list" :> Get '[HTML] (Html ()) :<|>
   -- /json/list
  "json" :> "list" :> Get '[JSON] [(FileName, FileSize)] :<|>
   -- /json/file?name=<filename>
  "json" :> QueryParam "name" Text :> "file" :> Get '[JSON] Text :<|>
   -- /json/tail?lines=<lines>&name=<filename>
  "json" :> "tail" :> QueryParam "lines" Int :> QueryParam "name" Text :> Get '[JSON] Text

itemApi :: Proxy LogFileApi
itemApi = Proxy

-- app
runApp :: IO ()
runApp = do
  args <- getArgs
  let defaultPath = "/var/log"
      defaultPort = 3000
      defaultHost = "127.0.0.1"
      (host, port, logPath) = case args of
                 (x:y:z:_) -> (x, read y :: Int, z)
                 (x:y:_) -> (x, read y :: Int, defaultPath)
                 (x:_) -> (x, defaultPort, defaultPath)
                 _ -> (defaultHost, defaultPort, defaultPath)
      conf = Conf { path = logPath
                   ,lastdays = 7
                  }
      settings =
        setPort port $
        setHost (fromString host) $
        setBeforeMainLoop (putStrLn ("listening on "
                                     ++ host
                                     ++ ":"
                                     ++ show port
                                     ++ " serving files in "
                                     ++ show logPath
                                    )) $
        defaultSettings
  runSettings settings =<< mkApp conf

mkApp :: Conf -> IO Application
mkApp c = return $ serve itemApi (server c)

-- the server that serves all the routes
server :: Conf -> Server LogFileApi
server c = showFileHTML
      :<|> tailFileHTML
      :<|> listFilesHTML c
      :<|> listFilesJSON c
      :<|> showFileJSON
      :<|> tailFileJSON

-- returns tail file content as a HTML page
tailFileHTML :: Maybe Int -> Maybe FileName -> Handler (Html ())
tailFileHTML (Just len) (Just fpIn) = do
  content <- getFileContent fpIn
  -- a much more efficient way of doing this would
  -- be to read the file backwards
  let ts = reverse . take len . reverse $ content
      page = filePage fpIn ts
  return page
tailFileHTML _ _ = return ""

-- returns tail file content as Text
tailFileJSON :: Maybe Int -> Maybe FileName -> Handler Text
tailFileJSON (Just len) (Just fpIn) = do
  content <- getFileContent fpIn
  let ts = T.concat . reverse . take len . reverse $ content
  return ts
tailFileJSON _ _ = return ""

-- returns file content as Text
showFileJSON :: Maybe FileName -> Handler Text
showFileJSON (Just fpIn) = do
  content <- getFileContent fpIn
  return (T.concat content)
showFileJSON _ = return ""

-- creates a HTML page of file content
showFileHTML :: Maybe FileName -> Handler (Html ())
showFileHTML (Just fpIn) = do
  content <- getFileContent fpIn
  let page = filePage fpIn content
  return page
showFileHTML _ = return (return ())

-- reads the content of a file
getFileContent :: FileName -> Handler [Text]
getFileContent fpIn = do
  -- security check, remove .. from path so
  -- we do not expose all files in the file system
  let fp = T.unpack (T.replace ".." "" fpIn)
  liftIO $ putStrLn $ "getting content of " <> fp
  -- we could check that the requested file is within the
  -- the give time period, but since this is an internal tool
  -- we skip that check.
  content <- liftIO (runConduitRes
                     $ C.sourceFile fp
                     .| C.decodeUtf8Lenient
                     .| C.sinkList)
  return (T.lines $ T.concat content)

-- fetch files
getLogFiles :: Conf -> Handler [LogFile]
getLogFiles conf = do
  currentTime <- liftIO getCurrentTime
  let timeCutOff = addUTCTime (nominalDay * (-(fromInteger (lastdays conf)))) currentTime
  -- fetch files recurisvely
  fs <- runConduitRes
     $ sourceDirectoryDeep False (path conf)
    .| C.mapM (\fp -> do
                       modTime <- liftIO (getModificationTime fp)
                       status <- liftIO (getFileStatus fp)
                       -- check modification time
                       let file = if modTime >= timeCutOff
                                  then Just $ LogFile (T.pack fp) status
                                  else Nothing
                       return file)
    .| C.sinkList
  return (catMaybes fs)

-- seconds in one day
nominalDay :: NominalDiffTime
nominalDay = 86400

-- app entry point
main :: IO ()
main = do
  putStrLn "logAPI started"
  runApp


-- returns list of files in the format [(filename, file size)]
listFilesJSON :: Conf -> Handler [(FileName, Integer)]
listFilesJSON conf = do
  files <- getLogFiles conf
  let xs = fmap (\x -> (filename x, toInteger (fileSize (filestatus x)))) files
  return xs

-- render a pages with the files listed
listFilesHTML :: Conf -> Handler (Html ())
listFilesHTML conf = do
  files <- getLogFiles conf
  let page = listPage conf files
  return page

-- HTML to show the file list page
listPage :: Conf -> [LogFile] -> Html ()
listPage conf files =
  doctypehtml_
    (do head_ (do meta_ [charset_ "utf-8"]
                  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
                  title_ "log files")
        body_ (div_ [class_ "container"]
                    (do h1_ "Logs"
                        ("Files changed last " <> toHtml (show (lastdays conf)) <> " days:")
                        hr_ []
                        when (0 /= 1)
                             (table_ (do
                                         tr_ (do td_ (strong_ "File")
                                                 td_ (strong_ "Size (bytes)")
                                             )
                                         (do mapM_ (\w -> tr_ $
                                                            (do
                                                                let name = filename w
                                                                    size = fileSize (filestatus w)
                                                                td_ (a_ [href_ ("file?name=" <> name)] (toHtml name))
                                                                td_ (toHtml (show size))
                                                                td_ (a_ [href_ ("tail?lines=100&name=" <> name)] (toHtml "tail"))

                                                            )) files
                                          )
                                     )
                             )
                        hr_ []
                        )))


-- HTML to show file content page
filePage :: FileName -> [Text] -> Html ()
filePage fileName ls =
  doctypehtml_
    (do head_ (do meta_ [charset_ "utf-8"]
                  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
                  title_ "log files")
        body_ (div_ [class_ "container"]
                    (do h1_ (toHtml fileName)
                        hr_ []
                        when (0 /= 1) (mapM_ (\x -> do
                                                      br_ []
                                                      toHtml x) ls)
                        hr_ []
                        )))

