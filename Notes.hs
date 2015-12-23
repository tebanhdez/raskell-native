module Notes where


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

<<<<<<< HEAD
noteServer :: Text -> TVar [Note] -> Server NoteAPI
noteServer home notes =
=======
server :: Text -> TVar [Note] -> Server NoteAPI
server home notes =
>>>>>>> 28317c8d8653fb2c0e89ab837a7de10d44badbd1
         return home
    :<|> getNotes notes
    :<|> postNote notes

