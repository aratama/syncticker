module SyncTicker.Main where

import Control.Bind (bind, discard, pure, (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Timer (setInterval)
import DOM.HTML (window)
import DOM.HTML.Location (search)
import DOM.HTML.Window (history, localStorage, location)
import DOM.WebStorage.Storage (getItem)
import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Unit (Unit, unit)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Query (action, liftEff)
import Halogen.VDom.Driver (runUI)
import Prelude (flip, id, show, ($), (<>))
import SyncTicker.Type (Effects, Query(..))
import SyncTicker.UI (ui)

main :: forall eff. Eff (Effects eff) Unit
main = do 

    s <- window >>= location >>= search

    timerID <- case regex "^\\?id=([a-zA-Z0-9]*?)$" noFlags of 

        Left err -> do 
            log ("internal error in Main")
            pure Nothing
            
        Right pattern -> case match pattern s >>= flip index 1 >>= id of 

            Nothing -> do 
                timerID <- window >>= localStorage >>= getItem "syncticker/timerID"
                log "regex error or no url query"
                log ("use id from local storage: " <> show timerID)
                pure timerID
                
            Just timerID -> do 
                log ("Initial Timer ID: " <> timerID)
                pure (Just timerID)

            
    runHalogenAff do
        
        body <- awaitBody
        io <- runUI ui timerID body

        io.query (action Initialize)

        _ <- liftEff $ setInterval 200 $ runHalogenAff do
            io.query (action Tick)

        pure unit




















