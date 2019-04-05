module Main where

import Prelude

import Concur.Core                          (Widget)
import Concur.Core.Types                    (andd)
import Concur.Core.FRP                      (Signal, display, dyn, hold)
import Concur.React                         (HTML)
import Concur.React.DOM                     (El', button, button', div,
                                             div_, div',
                                             input, nav, p', text)
import Concur.React.Props                   (_type, checked, classList,
                                             className, onChange,
                                             onClick, onFocus,
                                             unsafeTargetValue, value)
import Concur.React.Run                     (runWidgetInDom)
import Control.Alt                          ((<|>))
import Control.Monad.Rec.Class              (forever)
import Control.MultiAlternative             (orr)
import Data.Maybe                           (Maybe(..), fromMaybe)
import Data.Traversable                     (traverse)
import Data.Array                           ((..), (!!), length, zip)
import Data.Tuple                           (Tuple, fst, snd)
import Effect                               (Effect)
import Effect.Class                         (liftEffect)
import Effect.Class.Console                 (log)
import React.SyntheticEvent                 (SyntheticMouseEvent)


-- | This is currently a mockup
data Record = Record String

mkRecordWidget :: Record -> forall a. Widget HTML a
mkRecordWidget (Record recStr) =
  div' [ text (recStr <> " Sailor!") ]

rec1 = Record "foo"
rec2 = Record "bar"

recArray :: Array Record
recArray = [rec1, rec2]

mkRecArrayWidg :: Array Record ->  forall a. Widget HTML a
mkRecArrayWidg ra = div' $ map mkRecordWidget ra

main :: Effect Unit
main = runWidgetInDom "root" $ mkRecArrayWidg recArray
--main = runWidgetInDom "root" $ hello
