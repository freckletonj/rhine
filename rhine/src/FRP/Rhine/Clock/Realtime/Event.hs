{- |
This module provides two things:

* Clocks that tick whenever events arrive on a 'Control.Concurrent.Chan',
  and useful utilities.
* Primitives to emit events.

Note that _events work across multiple clocks_,
i.e. it is possible (and encouraged) to emit events from signals
on a different clock than the event clock.
This is in line with the Rhine philosophy that _event sources are clocks_.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module FRP.Rhine.Clock.Realtime.Event where

-- base
import Control.Concurrent.Chan
import Data.Time.Clock

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

-- rhine
import FRP.Rhine
import FRP.Rhine.Schedule.Concurrently

{- A simple example using events looks as follows:

@
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.SyncSF.Except

-- | A simple example that creates an event every second
--   and handles it by outputting it on the console.
type EventIO = EventChanT String IO
eventExample :: IO ()
eventExample = runEventChanT $ flow
  $ emitEventSystem **@ concurrentlyWithEvents @** handleEventSystem
  where
    emitEventSystem = safely emitEvents @@ liftClock waitClock
    emitEvents :: SyncExcept EventIO (HoistClock IO EventIO (Millisecond 10)) () () Empty
    emitEvents = do
      try $ timer_ 1
      once_ $ emit "Hello World!"
      emitEvents

    handleEventSystem = handleEvents @@ EventClock
    handleEvents :: MonadIO m => SyncSF m (EventClock String) () ()
    handleEvents = theTag >>> arrMSync (putStrLn >>> liftIO)
@
-}


-- * Monads allowing for event emission and handling

-- | A monad transformer in which events can be emitted onto a 'Chan'.
type EventChanT event m = ReaderT (Chan event) m


{- | Create a channel across which events can be communicated,
and subsequently execute all event effects on this channel.

Ideally, this action is run _outside_ of 'flow',
e.g. @runEventChanT $ flow myRhine@.
This way, exactly one channel is created.

Caution: Don't use this with 'liftMSFPurer',
since it would create a new channel every tick.
Instead, create one @chan :: Chan c@, e.g. with 'newChan',
and then use 'runEventChanS'.
-}
runEventChanT :: MonadIO m => EventChanT event m a -> m a
runEventChanT a = do
  chan <- liftIO $ newChan
  runReaderT a chan

{- | Remove ("run") an 'EventChanT' layer from the monad stack
by passing it explicitly the channel over which events are sent.

This is usually only needed if you can't use 'runEventChanT'
to create the channel.
Typically, create a @chan :: Chan c@ in your main program
before the main loop (e.g. 'flow') would be run,
then, by using this function,
pass the channel to every behaviour or 'SyncSF' that wants to emit events,
and, by using 'eventClockOn', to every clock that should tick on the event.
-}
runEventChanS
  :: Monad m
  => Chan event
  -> BehaviourF (EventChanT event m) td a b -> BehaviourF m td a b
runEventChanS chan behaviour = runReaderS_ behaviour chan
-- TODO Weirdly, can't refactor this using flip. (GHC bug?)

-- * Event emission

{- | Emit a single event.
This causes every 'EventClock' on the same monad to tick immediately.

Be cautious when emitting events from a signal clocked by an 'EventClock'.
Nothing prevents you from emitting more events than are handled,
causing the event buffer to grow indefinitely.
-}
emit :: MonadIO m => event -> EventChanT event m ()
emit event = do
  chan <- ask
  liftIO $ writeChan chan event

-- | Emit an event on every tick.
emitS :: MonadIO m => SyncSF (EventChanT event m) cl event ()
emitS = arrMSync emit

-- | Emit an event whenever the input value is @Just event@.
emitSMaybe :: MonadIO m => SyncSF (EventChanT event m) cl (Maybe event) ()
emitSMaybe = mapMaybe emitS >>> arr (const ())

-- * Event clocks and schedules

-- | A clock that ticks whenever an @event@ is emitted.
--   It is not yet bound to a specific channel,
--   since ideally, the correct channel is created automatically
--   by 'runEventChanT'.
--   If you want to create the channel manually and bind the clock to it,
--   use 'eventClockOn'.
data EventClock event = EventClock

instance Monoid (EventClock event) where
  mempty      = EventClock
  mappend _ _ = EventClock

instance MonadIO m => Clock (EventChanT event m) (EventClock event) where
  type Tag          (EventClock event) = event
  type TimeDomainOf (EventClock event) = UTCTime
  startClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      ( arrM_ $ do
          chan  <- ask
          event <- liftIO $ readChan chan
          time  <- liftIO $ getCurrentTime
          return (time, event)
      , initialTime
      )

-- | Create an event clock that is bound to a specific event channel.
--   This is usually only useful if you can't apply 'runEventChanT'
--   to the main loop (see 'runEventChanS').
eventClockOn
  :: MonadIO m
  => Chan event
  -> HoistClock (EventChanT event m) m (EventClock event)
eventClockOn chan = HoistClock
  { unhoistedClock = EventClock
  , monadMorphism  = flip runReaderT chan
  }

{- |
Given two clocks with an 'EventChanT' layer directly atop the 'IO' monad,
you can schedule them using concurrent GHC threads,
and share the event channel.

Typical use cases:

* Different subevent selection clocks
  (implemented i.e. with 'FRP.Rhine.Clock.Select')
  on top of the same main event source.
* An event clock and other event-unaware clocks in the 'IO' monad,
  which are lifted using 'liftClock'.
-}
concurrentlyWithEvents
  :: ( TimeDomainOf cl1 ~ TimeDomainOf cl2
     , Clock (EventChanT event IO) cl1
     , Clock (EventChanT event IO) cl2
     )
  => Schedule (EventChanT event IO) cl1 cl2
concurrentlyWithEvents = readerSchedule concurrently
