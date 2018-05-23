-- | A simple example that creates an event every second
--   and handles it by outputting it on the console.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- base
import Control.Concurrent
import Control.Monad (void)

-- transformers
-- TODO This is annoying
import Control.Monad.Trans.Reader (runReaderT)

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Event
import FRP.Rhine.Clock.Realtime.Millisecond

-- | The 'IO' monad,
--   augmented with the capability of sending and receiving events.
type EventIO = EventChanT String IO

-- | Emits an event with value "Hello World!" every second.
emitEvents :: SyncSF EventIO (HoistClock IO EventIO (Millisecond 1000)) () ()
emitEvents = arrMSync_ $ emit "Hello World!"

-- | Each time an event arrives, this function is called.
--   It simply outputs the event on the console.
handleEvents :: (MonadIO m, Tag cl ~ String) => SyncSF m cl () ()
handleEvents = theTag >>> arrMSync (putStrLn >>> liftIO)

-- | Run both subsystems in parallel.
eventExample :: IO ()
eventExample = runEventChanT $ flow
  $ emitEventSystem **@ concurrentlyWithEvents @** handleEventSystem
  where
    emitEventSystem   = emitEvents   @@ liftClock waitClock
    handleEventSystem = handleEvents @@ EventClock

-- | Run both subsystems in different threads.
threadsExample :: IO ()
threadsExample = do
  chan <- newChan
  void $ forkIO $ flow $ handleEvents @@ (eventClockOn chan :: HoistClock (EventChanT String IO) IO (EventClock String))
  flip runReaderT chan $ flow $ emitEvents @@ liftClock waitClock

main :: IO ()
main = do
  putStrLn $ unwords
    [ "Press Return to run eventExample."
    , "Press any nonempty string and Return to run threadsExample."
    ]
  s <- getLine
  if null s
    then eventExample
    else threadsExample
