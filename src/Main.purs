module Main where

import Prelude                              (Unit, discard, void, ($))

import Concur.Core                          (Widget)
import Concur.React                         (HTML)
import Concur.React.DOM                     (button, button', div', text)
import Concur.React.Props                   (onClick)
import Concur.React.Run                     (runWidgetInDom)
import Effect                               (Effect)

hello :: forall a. Widget HTML a
hello = do
  buttonHello
  text "Hello Sailor!"

buttonHello :: Widget HTML Unit
buttonHello = void $ button [onClick] [text "Say Hello"]

main :: Effect Unit
main = runWidgetInDom "root" hello
