module Performance where


import Control.Concurrent.STM
import Control.Monad.IO.Class

import qualified Data.Text as T

import Tracks
----
-- Performance
----
getPerformance :: MonadIO m => TVar [Track] -> T.Text -> m Track
getPerformance tracks requestId = return Track { trackId = "1",
                                           requestId = requestId,
                                           matches = [],
                                           performance = Performance 
                                                  { 
                                                  information = "info"
                                                  }
                                         }