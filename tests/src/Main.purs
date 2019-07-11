module Test.Main where

import Prelude

import Effect                            (Effect)
import Effect.Aff                        (Aff)
import Effect.Class                      (liftEffect)
import Effect.Console                    (logShow)
import Test.Data                         as TD
--import Test.Unit.Assert                  as Assert

import Metajelo                          (renderRecord)

main :: Effect Unit
main = do
  guiDemo

tlog :: forall a. Show a => a -> Aff Unit
tlog = liftEffect <<< logShow

guiDemo :: Effect Unit
guiDemo = renderRecord "metajelo_root" TD.metajeloXml
