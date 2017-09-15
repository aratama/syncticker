module SyncTicker.Type where

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (IntervalId, TIMER)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Maybe (Maybe)
import Data.Void (Void)
import Halogen.Aff.Effects (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import SyncTicker.Server (ServerState)
import Web.Firebase4.Type (FIREBASE, Firebase)

type TimerID = String

type State = {
    serverState :: Maybe ServerState,

    max :: Int, 
    count :: Int,
    active :: Boolean, 

    timerID :: TimerID,
    interval :: Maybe IntervalId, 
    help :: Boolean, 
    firebase :: Maybe Firebase
}

data Query a 
    = Initialize a 
    | Menu a
    | Backward a
    | Play a 
    | Pause a  
    | Tick a
    | Adjust Int a
    | Help a
    | CloseHelp a
    | Receive (Maybe ServerState) a
    | UpdateTimerID String a
     

type Input = String

type Output = Void

type Effects eff = HalogenEffects (
    ajax :: AJAX, 
    timer :: TIMER,
    firebase :: FIREBASE, 
    console :: CONSOLE,
    dom :: DOM, 
    history :: HISTORY | eff)











