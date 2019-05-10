module Test.Main where

import Prelude

import Control.Alt                        ((<|>))
import Control.Monad.Rec.Class            (forever)
import Control.MultiAlternative           (orr)

import Data.Array                        ((!!), length)
import Data.Array.NonEmpty               as DAN
import Data.Either                       (fromRight)
import Data.Maybe                        (Maybe(..), fromJust, isJust)
import Effect                            (Effect)
import Effect.Aff                        (Aff)
import Effect.Class                      (liftEffect)
import Effect.Console                    (logShow)
import Partial.Unsafe                    (unsafePartial)
import Test.Data                         as TD
import Test.Unit                         (suite, test)
import Test.Unit.Main                    (runTest)
import Test.Unit.Assert                  as Assert
import Text.Email.Validate               as EA
import URL.Validator                     as URL

import Metajelo                          (renderRecord)
import Metajelo.Types                    as MJ
import Metajelo.XPaths                   as MXP

main :: Effect Unit
main = do
  guiDemo

tlog :: forall a. Show a => a -> Aff Unit
tlog = liftEffect <<< logShow

guiDemo :: Effect Unit
guiDemo = renderRecord "metajelo_root" TD.metajeloXml
