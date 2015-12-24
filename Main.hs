import Control.Concurrent.STM
import Data.Proxy
import Data.Text
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import qualified Data.Text as T

import Tracks
import Songs
import Performance



type GeneralAPI = 
         Get Text
    :<|> "tracks" :> Get [Track]
    :<|> "tracks" :> ReqBody PostTrack :> Post [Track]
    :<|> "songs" :> Get [Song]
    :<|> "songs" :> ReqBody PostSong :> Post [Song]
    :<|> "performance" :> Capture "requestId" Text :> Get Track


generalAPI :: Proxy GeneralAPI
generalAPI =
    Proxy

server :: Text -> TVar [Track] -> TVar [Song] -> Server GeneralAPI
server home tracks songs =
         return home
    :<|> getTracks tracks
    :<|> postTrack tracks 
    :<|> getSongs songs
    :<|> postSong songs
    :<|> getPerformance tracks


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
    songs <- emptySongs
    run port $ serve generalAPI $ server home tracks songs





