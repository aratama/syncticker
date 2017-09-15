module SyncTicker.Client where 

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Halogen (HalogenM, SubscribeStatus(Listening), eventSource, subscribe)
import Prelude (Unit, void, ($), (>>=))
import SyncTicker.Server (ServerState)
import Web.Firebase4.App.App (database)
import Web.Firebase4.Database.Database (ref)
import Web.Firebase4.Database.Reference (on, set)
import Web.Firebase4.Database.Snapshot (val)
import Web.Firebase4.Type (EventType(Value), FIREBASE, Firebase)

setValue :: forall eff. ServerState -> Firebase -> Eff (firebase :: FIREBASE | eff) Unit
setValue value firebase = database firebase >>= ref "timers/example" >>= set (toForeign value)

subscribeValue :: forall m o p g f s eff
    . MonadAff (avar :: AVAR, console :: CONSOLE, firebase :: FIREBASE | eff) m
    => Firebase 
    -> (ServerState -> SubscribeStatus -> f SubscribeStatus) 
    -> HalogenM s f g p o m Unit
subscribeValue firebase f = subscribe (eventSource addListener onValue)
  where 
    onValue snapshot = Just (f snapshot Listening)
    addListener resolve = void $ database firebase >>= ref "timers/example" >>= on Value logShow \snapshot -> do
        resolve (unsafeFromForeign (val snapshot) :: ServerState)

