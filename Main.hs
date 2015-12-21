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

data Track = Track
    { 
      trackId :: Text
    }
  deriving (Generic, Show)

instance ToJSON Track


-- TODO
data Match = Match
    {
      title :: Text,
      probability :: Float
    }
    deriving (Generic, Show)

instance ToJSON Match


data Response = Response
    {
      requestId :: Text,
      matches :: Text
    }
    deriving (Generic, Show)

instance ToJSON Response

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

postTrack :: MonadIO m => Response
postTrack tracks post =
    liftIO $ do
      let response = Response
            {
              requestId = "1",
              matches = "ChangeTypeToList"
            }
        return response


type TrackAPI =
         Get Text
    :<|> "tracks" :> Get [Track]
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
