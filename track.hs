----
-- Tracks
----

data Track = Track
    { 
    timeStamp :: Text
    }
  deriving (Generic, Show)

instance ToJSON Track


newtype PostTrack = PostTrack
    { tracksContents :: Text
    }
  deriving Show

instance FromJSON PostTrack where
    parseJSON (Object o) = PostTrack <$> o .: "timeStamp"
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
      iso <- getISO8601DateTime
      T.putStrLn $ T.concat [iso, " ", tracksContents post]
      let track = Track
            { timeStamp = iso
            }
      atomically $ do
        oldTracks <- readTVar tracks
        let newTracks = track : oldTracks
        writeTVar tracks newTracks
        return newTracks


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

