module Tracks where


import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Hourglass
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.Hourglass
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L

----
-- Tracks
----

getMatches trackId = return [Match {title = "title 1", probability = 0.5},Match {title = "title 2", probability = 0.3}]                     

data Match = Match
    { 
    title :: Text,
    probability :: Float
    }
  deriving (Generic, Show)
instance ToJSON Match

data Performance = Performance
    { 
    information :: Text
    }
  deriving (Generic, Show)
instance ToJSON Performance

data Track = Track
    { 
    trackId :: Text,
    requestId :: Text,
    matches :: [Match],
    performance :: Performance
    }
  deriving (Generic, Show)
instance ToJSON Track


newtype PostTrack = PostTrack
    { tracksContents :: Text
    }
  deriving Show

instance FromJSON PostTrack where
    parseJSON (Object o) = PostTrack <$> o .: "trackId"
    parseJSON _          = mzero


emptyTracks :: IO (TVar [Track])
emptyTracks =
    newTVarIO []

getTracks :: MonadIO m => TVar [Track] -> m [Track]
getTracks tracks =
    liftIO $ readTVarIO tracks

postTrack :: MonadIO m => TVar [Track] -> PostTrack -> m [Track]
postTrack tracks post =
    liftIO $ do
      T.putStrLn $ T.concat [tracksContents post]
      let trackId = tracksContents post
      let track = Track
            { 
              trackId = trackId,
              requestId = "1234-1234-1234",
              matches = L.concat (getMatches trackId),
              performance = Performance {information = "some information goes here"}
            }
      atomically $ do
        oldTracks <- readTVar tracks
        let newTracks = track : oldTracks
        writeTVar tracks newTracks
        return newTracks


type TrackAPI ty= 
         Get Text
    :<|> "tracks" :> Get [ty]
    :<|> "tracks" :> ReqBody PostTrack :> Post [ty]


trackAPI :: Proxy (TrackAPI Track)
trackAPI =
    Proxy


trackServer :: Text -> TVar [Track] -> Server (TrackAPI Track)
trackServer home tracks =
         return home
    :<|> getTracks tracks
    :<|> postTrack tracks 