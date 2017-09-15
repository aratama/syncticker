module SyncTicker.Main where

import Control.Bind (bind, discard, pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (setInterval)
import Data.Unit (Unit, unit)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Query (action, liftEff)
import Halogen.VDom.Driver (runUI)
import Prelude (($))
import SyncTicker.Type (Effects, Query(..))
import SyncTicker.UI (ui)

main :: forall eff. Eff (Effects eff) Unit
main = do 

    runHalogenAff do
        
        body <- awaitBody
        io <- runUI ui "cats" body

        io.query (action Initialize)

        _ <- liftEff $ setInterval 1000 $ runHalogenAff do
            io.query (action Tick)

        pure unit

