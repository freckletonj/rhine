{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module FRP.Rhine.Clock
  ( module FRP.Rhine.Clock
  , module FRP.Rhine.TimeDomain
  , module Data.MonadicStreamFunction
  )
where

-- base
import qualified Control.Category as Category

-- transformers
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift, MonadTrans)

-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.TimeDomain

-- * The 'Clock' type class

{- |
A clock creates a stream of time stamps and additional information,
possibly together with side effects in a monad 'm'
that cause the environment to wait until the specified time is reached.
-}
type RunningClock m time tag = MSF m () (time, tag)

{- |
When starting a clock, the initial time is measured
(typically by means of a side effect),
and a running clock is returned.
-}
type RunningClockStarter m time tag = m (RunningClock m time tag, time)

{- |
Since we want to leverage Haskell's type system to annotate signal functions by their clocks,
each clock must be an own type, 'cl'.
Different values of the same clock type should tick at the same speed,
and only differ in implementation details.
Often, clocks are singletons.
-}
class TimeDomain (TimeDomainOf cl) => Clock m cl where
  -- | The time domain, i.e. type of the time stamps the clock creates.
  type TimeDomainOf cl
  -- | Additional information that the clock may output at each tick,
  --   e.g. if a realtime promise was met, if an event occurred,
  --   if one of its subclocks (if any) ticked.
  type Tag cl
  -- | The method that produces to a clock value a running clock,
  --   i.e. an effectful stream of tagged time stamps together with an initialisation time.
  startClock
    :: cl -- ^ The clock value, containing e.g. settings or device parameters
    -> RunningClockStarter m (TimeDomainOf cl) (Tag cl) -- ^ The stream of time stamps, and the initial time


-- * Auxiliary definitions and utilities

-- | An annotated, rich time stamp.
data TimeInfo cl = TimeInfo
  { -- | Time passed since the last tick
    sinceTick  :: Diff (TimeDomainOf cl)
    -- | Time passed since the initialisation of the clock
  , sinceStart :: Diff (TimeDomainOf cl)
    -- | The absolute time of the current tick
  , absolute   :: TimeDomainOf cl
    -- | The tag annotation of the current tick
  , tag        :: Tag cl
  }

-- | A utility that changes the tag of a 'TimeInfo'.
retag
  :: (TimeDomainOf cl1 ~ TimeDomainOf cl2)
  => (Tag cl1 -> Tag cl2)
  -> TimeInfo cl1 -> TimeInfo cl2
retag f TimeInfo {..} = TimeInfo { tag = f tag, .. }


-- | Given a clock value and an initial time,
--   generate a stream of time stamps.
genTimeInfo
  :: (Monad m, Clock m cl)
  => cl -> TimeDomainOf cl
  -> MSF m (TimeDomainOf cl, Tag cl) (TimeInfo cl)
genTimeInfo _ initialTime = proc (absolute, tag) -> do
  lastTime <- iPre initialTime -< absolute
  returnA                      -< TimeInfo
    { sinceTick  = absolute `diffTime` lastTime
    , sinceStart = absolute `diffTime` initialTime
    , ..
    }


-- * Certain universal building blocks to produce new clocks from given ones

-- ** Rescalings of time domains

-- | A pure morphism of time domains is just a function.
type Rescaling cl td = TimeDomainOf cl -> td

-- | An effectful morphism of time domains is a Kleisli arrow.
--   It can use a side effect to rescale a point in one time domain
--   into another one.
type RescalingM m cl td = TimeDomainOf cl -> m td

-- | An effectful, stateful morphism of time domains is an 'MSF'
--   that uses side effects to rescale a point in one time domain
--   into another one.
type RescalingS m cl td tag = MSF m (TimeDomainOf cl, Tag cl) (td, tag)

-- | Like 'RescalingS', but allows for an initialisation
--   of the rescaling morphism, together with the initial time.
type RescalingSInit m cl td tag = TimeDomainOf cl -> m (RescalingS m cl td tag, td)

-- | Convert an effectful morphism of time domains into a stateful one with initialisation.
--   Think of its type as @RescalingM m cl td -> RescalingSInit m cl td tag@,
--   although this type is ambiguous.
rescaleMToSInit
  :: Monad m
  => (td1 -> m td2) -> td1 -> m (MSF m (td1, tag) (td2, tag), td2)
rescaleMToSInit rescaling td1 = (arrM rescaling *** Category.id, ) <$> rescaling td1

-- ** Applying rescalings to clocks

-- | Applying a morphism of time domains yields a new clock.
data RescaledClock cl td = RescaledClock
  { unscaledClock :: cl
  , rescale       :: Rescaling cl td
  }


instance (Monad m, TimeDomain td, Clock m cl)
      => Clock m (RescaledClock cl td) where
  type TimeDomainOf (RescaledClock cl td) = td
  type Tag          (RescaledClock cl td) = Tag cl
  startClock (RescaledClock cl f) = do
    (runningClock, initTime) <- startClock cl
    return
      ( runningClock >>> first (arr f)
      , f initTime
      )

-- | Instead of a mere function as morphism of time domains,
--   we can transform one time domain into the other with an effectful morphism.
data RescaledClockM m cl td = RescaledClockM
  { unscaledClockM :: cl
  -- ^ The clock before the rescaling
  , rescaleM       :: RescalingM m cl td
  -- ^ Computing the new time effectfully from the old time
  }

instance (Monad m, TimeDomain td, Clock m cl)
      => Clock m (RescaledClockM m cl td) where
  type TimeDomainOf (RescaledClockM m cl td) = td
  type Tag          (RescaledClockM m cl td) = Tag cl
  startClock RescaledClockM {..} = do
    (runningClock, initTime) <- startClock unscaledClockM
    rescaledInitTime         <- rescaleM initTime
    return
      ( runningClock >>> first (arrM rescaleM)
      , rescaledInitTime
      )

-- | A 'RescaledClock' is trivially a 'RescaledClockM'.
rescaledClockToM :: Monad m => RescaledClock cl td -> RescaledClockM m cl td
rescaledClockToM RescaledClock {..} = RescaledClockM
  { unscaledClockM = unscaledClock
  , rescaleM       = return . rescale
  }


-- | Instead of a mere function as morphism of time domains,
--   we can transform one time domain into the other with a monadic stream function.
data RescaledClockS m cl td tag = RescaledClockS
  { unscaledClockS :: cl
  -- ^ The clock before the rescaling
  , rescaleS       :: RescalingSInit m cl td tag
  -- ^ The rescaling stream function, and rescaled initial time,
  --   depending on the initial time before rescaling
  }

instance (Monad m, TimeDomain td, Clock m cl)
      => Clock m (RescaledClockS m cl td tag) where
  type TimeDomainOf (RescaledClockS m cl td tag) = td
  type Tag          (RescaledClockS m cl td tag) = tag
  startClock RescaledClockS {..} = do
    (runningClock, initTime) <- startClock unscaledClockS
    (rescaling, rescaledInitTime) <- rescaleS initTime
    return
      ( runningClock >>> rescaling
      , rescaledInitTime
      )

-- | A 'RescaledClockM' is trivially a 'RescaledClockS'.
rescaledClockMToS
  :: Monad m
  => RescaledClockM m cl td -> RescaledClockS m cl td (Tag cl)
rescaledClockMToS RescaledClockM {..} = RescaledClockS
  { unscaledClockS = unscaledClockM
  , rescaleS       = rescaleMToSInit rescaleM
  }

-- | A 'RescaledClock' is trivially a 'RescaledClockS'.
rescaledClockToS
  :: Monad m
  => RescaledClock cl td -> RescaledClockS m cl td (Tag cl)
rescaledClockToS = rescaledClockMToS . rescaledClockToM

-- | Applying a monad morphism yields a new clock.
data HoistClock m1 m2 cl = HoistClock
  { unhoistedClock :: cl
  , monadMorphism  :: forall a . m1 a -> m2 a
  }

instance (Monad m1, Monad m2, Clock m1 cl)
      => Clock m2 (HoistClock m1 m2 cl) where
  type TimeDomainOf (HoistClock m1 m2 cl) = TimeDomainOf cl
  type Tag          (HoistClock m1 m2 cl) = Tag          cl
  startClock HoistClock {..} = do
    (runningClock, initialTime) <- monadMorphism $ startClock unhoistedClock
    let hoistMSF = liftMSFPurer
    -- TODO Look out for API changes in dunai here
    return
      ( hoistMSF monadMorphism runningClock
      , initialTime
      )

type LiftClock m t cl = HoistClock m (t m) cl

liftClock :: (Monad m, MonadTrans t) => cl -> LiftClock m t cl
liftClock unhoistedClock = HoistClock
  { monadMorphism = lift
  , ..
  }

type IOClock m cl = HoistClock IO m cl

ioClock :: MonadIO m => cl -> IOClock m cl
ioClock unhoistedClock = HoistClock
  { monadMorphism = liftIO
  , ..
  }
