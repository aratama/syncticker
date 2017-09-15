module SyncTicker.Server where

type ServerState = {
    max :: Int,
    count :: Int, 
    active :: Boolean
}
