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
  -- void to discard the button click
  void $ button [onClick] [text "Say Hello"]
  text "Hello Sailor!"

main :: Effect Unit
main = runWidgetInDom "root" hello
