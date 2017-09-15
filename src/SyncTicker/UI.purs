module SyncTicker.UI (ui) where

import Control.Applicative (pure, when)
import Control.Bind (bind, discard)
import Control.Monad.Aff (Aff)
import Control.Monad.State (put)
import Control.Monad.State.Class (get)
import Data.Foreign (unsafeFromForeign)
import Data.Maybe (Maybe(Nothing, Just), isJust)
import Data.NaturalTransformation (type (~>))
import Halogen (Component, liftEff)
import Halogen.Component (ComponentDSL, component)
import Halogen.HTML.Core (HTML)
import Prelude (max, min, negate, otherwise, unit, ($), (+), (-), (==))
import SyncTicker.Client (setValue, subscribeValue)
import SyncTicker.Render (render)
import SyncTicker.Server (ServerState)
import SyncTicker.Type (Effects, Input, Output, Query(..), State)
import Web.Firebase4 (initializeApp)
import Web.Firebase4.Database.Snapshot (val)
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

        subscribeValue firebase Update

        pure next

    Update value next -> do 

        state <- get
        put state { 
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
        pure next 


  
  where 
    updateServerValue = do 
        state <- get     
        liftEff case state.firebase of 
            Nothing -> pure unit 
            Just firebase -> setValue {
                max: state.max, 
                count: state.count, 
                active: state.active
            } firebase 

ui :: forall eff. Component HTML Query Input Output (Aff (Effects eff))
ui = component {
    render,
    eval,
    initialState: \_ -> { 
        timerID: "12345", 
        max: 0, 
        count: 0, 
        interval: Nothing,
        active: false, 
        help: true, 
        firebase: Nothing
    },
    receiver: \_ -> Nothing
}






































