module Lib (tick) where

import           Control.Monad.State (MonadState (get, put))

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)
