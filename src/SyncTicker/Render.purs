module SyncTicker.Render (render) where

import Data.Int (toNumber)
import Data.Ord (abs, (<), (<=))
import Data.Unit (Unit)
import Data.Void (Void)
import Halogen.HTML (ClassName(ClassName), HTML, IProp, PropName(PropName), h1_, h2_, i, input, p_, span, text)
import Halogen.HTML.Core (prop)
import Halogen.HTML.Elements (button, div)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_, classes, value)
import Prelude (div) as Prelude
import Prelude (max, min, mod, negate, not, show, (&&), (*), (/), (<>), (==), (||))
import SyncTicker.Classes (digit) as Classes
import SyncTicker.Classes (display, colon, controls, progress, triangle, up, down, bar, root, over, empty, disabled, help, close, hidden, dialog, timerIDClass)
import SyncTicker.Type (State, Query(..))
import Text.Format (format, precision)
import Unsafe.Coerce (unsafeCoerce)

render :: State -> HTML Void (Query Unit)
render state = div [class_ root] [

    div [classes [display, if 0 <= state.count then empty else over]] [
        digit d0 600, 
        digit d1 60, 
        div [class_ colon] [text ":"], 
        digit d2 10, 
        digit d3 1
    ],

    div [class_ controls] [
        
        button [ 
            onClick (input_ Backward), 
            class_ if state.active || state.count == state.max then disabled else empty 
        ] [icon "fast-backward"],
        
        button [
            onClick (input_ Play), 
            class_ if state.active then disabled else empty
        ] [icon "play"],
        
        button [
            onClick (input_ Pause), 
            class_ if state.active then empty else disabled
        ] [icon "pause"],
        
        button [
            onClick (input_ Help)
        ] [icon "question"]    
    
    ], 

    div [class_ progress] [
        div [
            class_ bar, 
            style ("width: " <> format (precision 2) (100.0 * toNumber (max 0 state.count) / toNumber state.max) <> "%")
        ] [] 
    ], 

    renderHelp
]
  where 
    style :: forall r i. String -> IProp (style :: String | r) i
    style str = unsafeCoerce (prop (PropName "style") str)

    icon name = i [class_ (ClassName ("fa fa-" <> name))] []

    timerID = "12345"
    count = abs (max (negate 5999) (min 5999 state.count))
    minutes = Prelude.div count 60 
    seconds = mod count 60

    d0 = Prelude.div minutes 10 
    d1 = mod minutes 10 
    d2 = Prelude.div seconds 10 
    d3 = mod seconds 10      
  
    digit :: forall p i. Int -> Int -> HTML p (Query Unit)
    digit n shift = div [class_ Classes.digit] [
        
        div [
            classes [triangle, up, buttonDisabled], 
            onClick (input_ (Adjust shift))
        ] [text "▲"],
        
        div [
            class_ (ClassName "number")
        ] [text (show n)],
        
        div [
            classes [triangle, down, buttonDisabled], 
            onClick (input_ (Adjust (negate shift)))
        ] [text "▼"]
    
    ] 
      where 
        buttonDisabled = if not state.active && state.count == state.max then empty else disabled

    renderHelp = div [classes [help, if state.help then empty else hidden]] [
        div [class_ dialog] [
            h1_ [text "Sync Ticker"], 
            h2_ [text "遠隔操作可能スピーチタイマー"], 
            p_ [text "他のデバイスとタイマーを共有するには、"],
            p_ [text "双方に同じタイマーIDを入力してください。"],
            div [class_ timerIDClass] [
                span [] [text "タイマーID"],
                input [value state.timerID] 
            ],
            button [class_ close, onClick (input_ CloseHelp)] [text "既存のタイマーに接続する"]
        ]
    ]














