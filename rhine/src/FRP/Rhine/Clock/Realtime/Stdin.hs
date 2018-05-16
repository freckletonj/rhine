{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module FRP.Rhine.Clock.Realtime.Stdin where

-- base
import Data.Time.Clock

-- transformers
import Control.Monad.IO.Class

-- rhine
import FRP.Rhine

{- |
A clock that ticks for every line entered on the console,
outputting the entered line as its |Tag|.
-}
data StdinClock = StdinClock

instance MonadIO m => Clock m StdinClock where
  type TimeDomainOf StdinClock = UTCTime
  type Tag          StdinClock = String

  startClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      (     arrM_ (liftIO getCurrentTime)
        &&& arrM_ (liftIO getLine)
      , initialTime
      )

instance Monoid StdinClock where
  mempty      = StdinClock
  mappend _ _ = StdinClock
