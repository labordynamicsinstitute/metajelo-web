module Main where

import Prelude

import Concur.Core                          (Widget)
import Concur.React                         (HTML)
import Concur.React.DOM                     (button, button', div', text)
import Concur.React.Props                   (onClick)
import Concur.React.Run                     (runWidgetInDom)
import Control.Alt                          ((<|>))
import Control.MultiAlternative             (orr)
import Effect                               (Effect)

hello :: forall a. Widget HTML a
hello = do
  greeting <- orr
    [ "Hello" <$ button [onClick] [text "Say Hello"]
    , "Namaste" <$ button [onClick] [text "Say Namaste"]
    ]
  text (greeting <> " Sailor!")

buttonHello :: Widget HTML Unit
buttonHello = void $ button [onClick] [text "Say Hello"]

main :: Effect Unit
main = runWidgetInDom "root" hello
