module Songs where


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
-- Songs
----

data Song = Song
    { 
    songTitle :: Text,
    artist :: Text,
    timeStamp :: Text
    }
  deriving (Generic, Show)
instance ToJSON Song

newtype PostSong = PostSong
    { songsContents :: Text
    }
  deriving Show

instance FromJSON PostSong where
    parseJSON (Object o) = PostSong <$> o .: "songTitle"
    parseJSON _          = mzero


emptySongs :: IO (TVar [Song])
emptySongs =
    newTVarIO []

getSongs :: MonadIO m => TVar [Song] -> m [Song]
getSongs songs =
    liftIO $ readTVarIO songs

postSong :: MonadIO m => TVar [Song] -> PostSong -> m [Song]
postSong songs post =
    liftIO $ do
      iso <- getISO8601DateTime
      T.putStrLn $ T.concat [iso, " ", postContents post]
      let song = Song
            { songTitle = postContents post,
              artist = postContents post,
              timeStamp = iso
            }
      atomically $ do
        oldSongs <- readTVar songs
        let newSongs = song : oldSongs
        writeTVar songs newSongs
        return newSongs


type SongAPI = 
         Get Text
    :<|> "songs" :> Get [Song]
    :<|> "songs" :> ReqBody PostSong :> Post [Song]


songAPI :: Proxy SongAPI
songAPI =
    Proxy


songServer :: Text -> TVar [Song] -> Server SongAPI
songServer home songs =
         return home
    :<|> getSongs songs
    :<|> postSong songs 