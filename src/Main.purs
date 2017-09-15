module Main where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import SyncTicker.Type (Effects)
import SyncTicker.Main (main) as SyncTicker

main :: forall eff. Eff (Effects eff) Unit
main = SyncTicker.main