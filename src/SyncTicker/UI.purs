module SyncTicker.UI (ui) where

import Control.Applicative (pure, when)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Now (now)
import Control.Monad.State (put)
import Control.Monad.State.Class (get)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Location (origin, pathname)
import DOM.HTML.Window (history, localStorage, location)
import DOM.WebStorage.Storage (setItem)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.NaturalTransformation (type (~>))
import Data.Nullable (toMaybe, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Halogen (Component, liftEff)
import Halogen.Component (ComponentDSL, component)
import Halogen.HTML.Core (HTML)
import Prelude (div, max, min, negate, otherwise, unit, ($), (+), (-), (/=), (<>), (==))
import SyncTicker.Client (setValue, subscribeValue, unsubscribeValue)
import SyncTicker.Render (render)
import SyncTicker.Type (Effects, Input, Output, Query(..), State)
import Web.Firebase4 (initializeApp)
import Web.Firebase4.Database.ServerValue (timestamp)
import Web.Firebase4.Type (Options(Options))

eval :: forall eff. Query ~> ComponentDSL State Query Output (Aff (Effects eff))
eval = case _ of

    Initialize next -> do 

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
                time <- liftEff now
                put state { 
                    serverState = serverState,
                    count = value.count, 
                    max = value.max, 
                    active = if value.active then Just { 
                            start: time, 
                            count: value.count 
                        } else Nothing
                }

        pure next
    
    Menu next -> do
        pure next 

    Backward next -> do
        state <- get
        put state { 
            count = max (negate 5999) (min 5999 (state.max)), 
            active = Nothing 
        }
        updateServerValue
        pure next

    Play next -> do
        state <- get
        time <- liftEff now
        put state { active = Just { start: time, count: state.count } }
        updateServerValue
        pure next 

    Pause next -> do
        state <- get
        put state { active = Nothing }
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
        case state.active of 
            Just { start, count } -> do
                time <- liftEff now
                let Milliseconds delta = unInstant time - unInstant start
                let count' = max (negate 5999) (min 5999 (count - div (floor delta) 1000))
                when (state.count /= count') do
                    put state { count = count' }
            Nothing -> pure unit 
        pure next

    Adjust delta next -> do
        state <- get
        case state.active of 
            Just time -> do 
                let count = max 0 (min state.max (state.count + delta))
                put state { count = count }
                updateServerValue

            Nothing | state.count == state.max -> do 

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
        liftEff do 
            
            case state.firebase of 
                Nothing -> pure unit 
                Just firebase -> do 
                    setValue state.timerID {
                        max: state.max, 
                        count: state.count, 
                        active: isJust state.active
                    } firebase 

            window >>= localStorage >>= setItem "syncticker/timerID" state.timerID


ui :: forall eff. Component HTML Query Input Output (Aff (Effects eff))
ui = component {
    render,
    eval,
    initialState: \timerID -> { 
        serverState: Nothing,
        timerID: fromMaybe "example" timerID, 

        max: 120, 
        count: 120, 
        active: Nothing,

        interval: Nothing,


        help: true, 
        firebase: Nothing
    },
    receiver: \_ -> Nothing
}


















