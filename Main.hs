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

import Tracks
import Notes
import Songs


{-
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
    run port $ serve trackAPI $ trackServer home tracks

-}


type GeneralAPI = 
	"testAPI1" :> test1API
	--"testAPI2" :> test2API

server :: server GeneralAPI
server = test1Server -- :<|> test2Server

app ::Application
app = serve generalAPI server

runServer :: Port -> IO()
runServer port = run port app

main :: IO()
main = runServer 8080





