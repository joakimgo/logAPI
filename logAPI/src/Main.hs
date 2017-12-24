{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Lens
import           Data.Aeson
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory (getModificationTime)
import           System.Posix.Files (fileSize, getFileStatus, FileStatus (..))
import           System.Environment (getArgs)
import           Data.Time.Clock
import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as BS
import Servant.HTML.Lucid
import Data.Conduit
import Lucid
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Combinators (sourceDirectoryDeep
                                ,sourceFileBS
                                ,sinkFile
                                ,sinkFileBS)

data Conf = Conf { path :: FilePath
                  ,lastdays :: Integer
                 } deriving (Show)

-- * file representation
data LogFile = LogFile { filename :: String
                        ,filestatus :: FileStatus
                       }

-- * api
type LogFileApi =
  "file" :> QueryParam "name" Text :> Get '[HTML] (Html ()) :<|>
  "tail" :> QueryParam "lines" Int :> QueryParam "name" Text :> Get '[HTML] (Html ()) :<|>
  "list" :> Get '[HTML] (Html ())

itemApi :: Proxy LogFileApi
itemApi = Proxy

-- * app
runApp :: IO ()
runApp = do
  args <- getArgs
  let -- defaultPath = "/var/log"
      defaultPath = "/opt/app"
      (port, logPath) = case args of
                 (x:y:_) -> (read x :: Int, y)
                 (x:_) -> (read x :: Int, defaultPath)
                 _ -> (3000, defaultPath)
      conf = Conf { path = logPath
                   ,lastdays = 7
                  }
      settings =
        setPort port $
        setBeforeMainLoop (putStrLn ("listening on port "
                                     ++ show port
                                     ++ " serving files in "
                                     ++ show logPath
                                    )) $
        defaultSettings
  runSettings settings =<< mkApp conf

mkApp :: Conf -> IO Application
mkApp c = return $ serve itemApi (server c)

server :: Conf -> Server LogFileApi
server c = showFile
      :<|> tailFile
      :<|> listFiles c

tailFile :: Maybe Int -> Maybe Text -> Handler (Html ())
tailFile (Just len) (Just fpIn) = do
  content <- getFileContent fpIn
  let ts = reverse . take len . reverse $ content
      page = filePage fpIn ts
  return page
tailFile _ _ = return ""

showFile :: Maybe Text -> Handler (Html ())
showFile (Just fpIn) = do
  content <- getFileContent fpIn
  let page = filePage fpIn content
  return page
showFile _ = return (return ())

getFileContent :: Text -> Handler [Text]
getFileContent fpIn = do
  let fp = T.unpack (T.replace ".." "" fpIn)
  liftIO $ putStrLn $ "getting content of " <> fp
  content <- liftIO (runConduitRes
                     $ C.sourceFile fp
                     .| C.decodeUtf8Lenient
                     .| C.sinkList)
  return (T.lines $ T.concat content)

-- todo, add comments
getLogFiles :: Conf -> Handler [LogFile]
getLogFiles conf = do
  currentTime <- liftIO getCurrentTime
  let timeCutOff = addUTCTime (nominalDay * (-(fromInteger (lastdays conf)))) currentTime
  fs <- runConduitRes
     $ sourceDirectoryDeep False (path conf)
    .| C.mapM (\fp -> do
                       modTime <- liftIO (getModificationTime fp)
                       status <- liftIO (getFileStatus fp)
                       let file = if modTime >= timeCutOff
                                  then Just (LogFile fp status)
                                  else Nothing
                       return file)
    .| C.sinkList
  return (catMaybes fs)

nominalDay :: NominalDiffTime
nominalDay = 86400


main :: IO ()
main = do
  putStrLn "logAPI started"
  runApp


listFiles :: Conf -> Handler (Html ())
listFiles conf = do
  files <- getLogFiles conf
  let page = listPage conf files
  return page


listPage :: Conf -> [LogFile] -> Html()
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
                                                                let name = T.pack (filename w)
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

filePage :: Text -> [Text] -> Html ()
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
                                                      toHtml ( x)) ls)
                        hr_ []
                        )))

