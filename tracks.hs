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
              requestId = tracksContents post,
              matches = L.concat (getMatches trackId),
              performance = Performance {information = "some information here"}
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

