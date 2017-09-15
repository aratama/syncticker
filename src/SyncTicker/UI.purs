module SyncTicker.UI (ui) where

import Control.Applicative (pure, void, when)
import Control.Bind (bind, discard)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log)
import Control.Monad.State (put)
import Control.Monad.State.Class (get)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Location (origin, pathname, search)
import DOM.HTML.Window (history, location)
import Data.Array (index)
import Data.Either (Either(..))
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.NaturalTransformation (type (~>))
import Data.String (Pattern(..), split)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Halogen (Component, liftEff)
import Halogen.Component (ComponentDSL, component)
import Halogen.HTML.Core (HTML)
import Prelude (max, min, negate, otherwise, unit, ($), (+), (-), (<>), (==))
import SyncTicker.Client (setValue, subscribeValue, unsubscribeValue)
import SyncTicker.Render (render)
import SyncTicker.Server (ServerState)
import SyncTicker.Type (Effects, Input, Output, Query(..), State)
import Web.Firebase4 (initializeApp)
import Web.Firebase4.Database.Snapshot (val)
import Web.Firebase4.Type (Options(Options))

eval :: forall eff. Query ~> ComponentDSL State Query Output (Aff (Effects eff))
eval = case _ of

    Initialize next -> do 

    
        s <- liftEff do  
            w <- window
            h <- history w
            l <- location w
            search l 

        case regex "^\\?id=([a-zA-Z0-9]*?)$" noFlags of 
            Left err -> liftEff do 
                log "regex error"
            Right pattern -> fromMaybe (pure unit) do 
                m <- match pattern s 
                timerIDMaybe <- index m 1
                timerID <- timerIDMaybe
                Just do 
                    st <- get
                    put st { timerID = timerID } 
                    liftEff (log ("Initial Timer ID: " <> timerID))

        state <- get 

        firebase <- liftEff $ initializeApp (Options {
            apiKey: "AIzaSyAXEv-3KXVncxQMlhIUJ1UYMU4g2CrnhOs",
            authDomain: "synchronicity-timer.firebaseapp.com",
            databaseURL: "https://synchronicity-timer.firebaseio.com",
            --projectId: "synchronicity-timer",
            storageBucket: "",
            messagingSenderId: "319359458223"
        }) Nothing

        put state { firebase = Just firebase }

        updateTimerID state.timerID

        pure next

    Receive serverState next -> do 

        state <- get
        case serverState of 
            Nothing -> put state { 
                    serverState = serverState
                }
            Just value -> do 
                put state { 
                    serverState = serverState,
                    count = value.count, 
                    max = value.max, 
                    active = value.active
                }

        pure next
    
    Menu next -> do
        pure next 

    Backward next -> do
        state <- get
        put state { 
            count = max (negate 5999) (min 5999 (state.max)), 
            active = false 
        }
        updateServerValue
        pure next

    Play next -> do
        state <- get
        let value = state { active = true }
        put value
        updateServerValue
        pure next 

    Pause next -> do
        state <- get
        put state { active = false }
        updateServerValue
        pure next 

    Help next -> do
        state <- get
        put state { 
            help = true 
        }
        pure next

    Tick next -> do
        state <- get
        when state.active do
            put state { 
                count = max (negate 5999) (min 5999 (state.count - 1)) 
            }
        pure next

    Adjust delta next -> do
        state <- get
        case state.active of 
            true -> do 
                let count = max 0 (min state.max (state.count + delta))
                put state { count = count }
                updateServerValue

            false | state.count == state.max -> do 

                    let count = max 0 (min 5999 (state.max + delta))
                    put $ state { 
                        max = count, 
                        count = count
                    }
                    updateServerValue

                  | otherwise -> pure unit
        pure next 

    CloseHelp next -> do 
        state <- get
        put state { 
            help = false 
        }
        
        -- create new timer
        liftEff case state.firebase of 
            Nothing -> pure unit 
            Just firebase -> setValue state.timerID {
                max: state.max, 
                count: state.count, 
                active: state.active
            } firebase 

        pure next 

    UpdateTimerID timerID next -> do
        updateTimerID timerID    
        pure next

  
  where 
    updateTimerID timerID = do 
        state <- get 

        case state.firebase of 

            Just firebase -> do 

                liftEff $ unsubscribeValue state.timerID firebase
                liftEff $ log $ "unsubscribe " <> state.timerID

                subscribeValue timerID firebase Receive
                liftEff $ log $ "subscribe " <> timerID

                liftEff do 
                    w <- window
                    h <- history w
                    l <- location w
                    o <- origin l
                    p <- pathname l
                    let url = URL (o <> p <> "?id=" <> timerID)
                    pushState (toForeign {}) (DocumentTitle "") url h

                put state { 
                    timerID = timerID 
                }
                --updateServerValue
    
            _ -> pure unit 

    updateServerValue = do 
        state <- get     
        liftEff case state.firebase of 
            Nothing -> pure unit 
            Just firebase -> setValue state.timerID {
                max: state.max, 
                count: state.count, 
                active: state.active
            } firebase 


ui :: forall eff. Component HTML Query Input Output (Aff (Effects eff))
ui = component {
    render,
    eval,
    initialState: \_ -> { 
        serverState: Nothing,
        timerID: "example", 
        max: 0, 
        count: 0, 
        interval: Nothing,
        active: false, 
        help: true, 
        firebase: Nothing
    },
    receiver: \_ -> Nothing
}
