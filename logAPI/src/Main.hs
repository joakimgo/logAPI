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

-- * file representation
data LogFile = LogFile {
                         filename :: String
                        ,filestatus :: FileStatus
                       }

-- * api
-- todo, add tail endpoint
type LogFileApi =
  "file" :> QueryParam "name" Text :> Get '[PlainText] Text :<|>
  "list" :> Get '[HTML] (Html ())

itemApi :: Proxy LogFileApi
itemApi = Proxy

-- * app
-- todo, read port from command line
runApp :: IO ()
runApp = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server LogFileApi
server = getFileContent :<|> listFiles

getFileContent :: Maybe Text -> Handler Text
getFileContent Nothing = return ""
getFileContent (Just fpIn) = do
  let fp = "/var/log/" <> T.unpack (T.replace ".." "" fpIn)
  liftIO $ putStrLn $ "getting content of " <> fp
  content <- liftIO (runConduitRes
                     $ C.sourceFile fp
                     .| C.decodeUtf8Lenient
                     .| C.sinkList)
  return (T.unlines content)

-- todo, add comments
getLogFiles :: Handler [LogFile]
getLogFiles = do
  currentTime <- liftIO getCurrentTime
  let timeCutOff = addUTCTime (nominalDay * (-7)) currentTime
  fs <- runConduitRes
     $ sourceDirectoryDeep False "/var/log"
    .| C.mapM (\fp -> do
                       modTime <- liftIO (getModificationTime fp)
                       status <- liftIO (getFileStatus fp)
                       let file = if modTime >= timeCutOff
                                  then Just (LogFile fp status)
                                  else Nothing
                       return file)
    .| C.sinkList
  return (catMaybes fs)
  where -- | One day in 'NominalDiffTime'.
         nominalDay :: NominalDiffTime
         nominalDay = 86400


main :: IO ()
main = do
  putStrLn "logAPI started"
  runApp


listFiles :: Handler (Html ())
listFiles = do
  files <- getLogFiles
  let page = listPage files
  return page


listPage :: [LogFile] -> Html()
listPage files =
  doctypehtml_
    (do head_ (do meta_ [charset_ "utf-8"]
                  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
                  title_ "log files")
        body_ (div_ [class_ "container"]
                    (do h1_ "/var/log/"
                        "Files changed last 7 days:"
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
                                                                td_ (a_ [href_ ("tail?len=100&name=" <> name)] (toHtml "tail"))

                                                            )) files
                                          )
                                     )
                             )
                        hr_ []
                        )))
