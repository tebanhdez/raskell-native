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


getISO8601DateTime :: IO Text
getISO8601DateTime = do
    seconds <- timeCurrent
    let iso = timePrint ISO8601_DateAndTime seconds
    return $ T.pack iso


data Note = Note
    { contents :: Text
    , dateTime :: Text
    }
  deriving (Generic, Show)

instance ToJSON Note


newtype PostNote = PostNote
    { postContents :: Text
    }
  deriving Show

instance FromJSON PostNote where
    parseJSON (Object o) = PostNote <$> o .: "contents"
    parseJSON _          = mzero


emptyNotes :: IO (TVar [Note])
emptyNotes =
    newTVarIO []

getNotes :: MonadIO m => TVar [Note] -> m [Note]
getNotes notes =
    liftIO $ readTVarIO notes

postNote :: MonadIO m => TVar [Note] -> PostNote -> m [Note]
postNote notes post =
    liftIO $ do
      iso <- getISO8601DateTime
      T.putStrLn $ T.concat [iso, " ", postContents post]
      let note = Note
            { contents = postContents post
            , dateTime = iso
            }
      atomically $ do
        oldNotes <- readTVar notes
        let newNotes = note : oldNotes
        writeTVar notes newNotes
        return newNotes


type NoteAPI =
         Get Text
    :<|> "notes" :> Get [Note]
    :<|> "notes" :> ReqBody PostNote :> Post [Note]

noteAPI :: Proxy NoteAPI
noteAPI =
    Proxy

server :: Text -> TVar [Note] -> Server NoteAPI
server home notes =
         return home
    :<|> getNotes notes
    :<|> postNote notes


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


data Song = Song
    { 
    title :: Text
    }
  deriving (Generic, Show)
instance ToJSON Song


newtype PostTrack = PostTrack
    { tracksContents :: Text
    }
  deriving Show

instance FromJSON PostTrack where
    parseJSON (Object o) = PostTrack <$> o .: "trackId"
    parseJSON _          = mzero


emptySongs :: IO (TVar [Song])
emptySongs =
    newTVarIO []

getSongs :: MonadIO m => TVar [Song] -> m [Song]
getSongs songs =
    liftIO $ readTVarIO songs

postTrack :: MonadIO m => TVar [Track] -> PostTrack -> m [Track]
postTrack tracks post =
    liftIO $ do
      T.putStrLn $ T.concat [tracksContents post]
      let trackId = tracksContents post
      let track = Track
            { 
              trackId = trackId,
              requestId = "1234-ABCD-1234",
              matches = L.concat (getMatches trackId),
              performance = Performance {information = "some information here"}
            }
      atomically $ do
        oldTracks <- readTVar tracks
        let newTracks = track : oldTracks
        writeTVar tracks oldTracks
        return newTracks


type TrackAPI =
         Get Text
    :<|> "tracks" :> Get [Song]
    :<|> "tracks" :> ReqBody PostTrack :> Post [Track]


trackAPI :: Proxy TrackAPI
trackAPI =
    Proxy


serverTrack :: Text -> TVar [Track] -> Server TrackAPI
serverTrack home tracks =
         return home
    :<|> getTracks tracks
    :<|> postTrack tracks




main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Welcome to Haskell on Heroku" T.pack $
                 lookup "TUTORIAL_HOME" env
    --notes <- emptyNotes
    --run port $ serve noteAPI $ server home notes
    tracks <- emptyTracks
    run port $ serve trackAPI $ serverTrack home tracks
