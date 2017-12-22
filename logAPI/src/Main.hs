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
import           Control.Lens
import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory (getModificationTime)
import           Data.Time.Clock
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Combinators (sourceDirectoryDeep
                                ,sourceFileBS
                                ,sinkFile
                                ,sinkFileBS)

-- * file representation
data LogFile = LogFile {
                         filename :: String
                        ,content :: [Text]
                       } deriving (Show, Generic, ToJSON)

-- * api
-- todo, add list endpoint
type LogFileApi =
  "logfiles" :> Get '[JSON] [LogFile]

itemApi :: Proxy LogFileApi
itemApi = Proxy

-- * app
runApp :: IO ()
runApp = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

server :: Server LogFileApi
server = getLogFiles


-- todo, add a cache
-- add comments
getLogFiles :: Handler [LogFile]
getLogFiles = do
  currentTime <- liftIO getCurrentTime
  let timeCutOff = addUTCTime (nominalDay * (-7)) currentTime
  runConduitRes
     $ sourceDirectoryDeep False "."
    .| C.filterM (\fp -> do
                           modTime <- liftIO (getModificationTime fp)
                           return (modTime >= timeCutOff))
    .| C.mapM (\fp -> do
                         content <- liftIO (runConduitRes
                                            $ C.sourceFile fp
                                            .| C.decodeUtf8Lenient
                                            .| C.sinkList)
                         return (LogFile fp content))
    .| C.sinkList
  where -- | One day in 'NominalDiffTime'.
         nominalDay :: NominalDiffTime
         nominalDay = 86400


main :: IO ()
main = do
  putStrLn "Hello from logAPI"
  runApp


mkApp :: IO Application
mkApp = return $ serve itemApi server

