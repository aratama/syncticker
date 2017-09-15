module SyncTicker.Client where 

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Foreign (isNull, toForeign, unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Halogen (HalogenM, SubscribeStatus(Listening), eventSource, subscribe)
import Prelude (Unit, void, ($), (<>), (>>=))
import SyncTicker.Server (ServerState)
import SyncTicker.Type (TimerID)
import Web.Firebase4.App.App (database)
import Web.Firebase4.Database.Database (ref)
import Web.Firebase4.Database.Reference (off, on, set)
import Web.Firebase4.Database.Snapshot (val)
import Web.Firebase4.Type (EventType(Value), FIREBASE, Firebase)

setValue :: forall eff. TimerID -> ServerState -> Firebase -> Eff (firebase :: FIREBASE | eff) Unit
setValue timerID value firebase = database firebase >>= ref ("timers/" <> timerID) >>= set (toForeign value)

subscribeValue :: forall m o p g f s eff
    . MonadAff (avar :: AVAR, console :: CONSOLE, firebase :: FIREBASE | eff) m
    => TimerID
    ->  Firebase 
    -> (Maybe ServerState -> SubscribeStatus -> f SubscribeStatus) 
    -> HalogenM s f g p o m Unit
subscribeValue timerID firebase f = subscribe (eventSource addListener onValue)
  where 
    onValue snapshot = Just (f snapshot Listening)
    addListener resolve = void $ database firebase >>= ref ("timers/" <> timerID) >>= on Value logShow \snapshot -> do
        let foreignValue = val snapshot
        resolve if isNull foreignValue then Nothing else Just (unsafeFromForeign foreignValue :: ServerState)

unsubscribeValue :: forall eff. TimerID -> Firebase -> Eff (firebase :: FIREBASE | eff) Unit
unsubscribeValue timerID firebase = do
    database firebase >>= ref ("timers/" <> timerID) >>= off


