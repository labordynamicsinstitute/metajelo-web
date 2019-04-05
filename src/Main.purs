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
import Metajelo.Types                       as MJ
import React.SyntheticEvent                 (SyntheticMouseEvent)


mkRecordWidget :: MJ.Record -> forall a. Widget HTML a
mkRecordWidget (MJ.Record recStr) =
  div' [ text (recStr <> " Sailor!") ]

rec1 = MJ.Record "foo"
rec2 = MJ.Record "bar"

recArray :: Array MJ.Record
recArray = [rec1, rec2]

mkRecArrayWidg :: Array MJ.Record ->  forall a. Widget HTML a
mkRecArrayWidg ra = div' $ map mkRecordWidget ra

main :: Effect Unit
main = runWidgetInDom "root" $ mkRecArrayWidg recArray
--main = runWidgetInDom "root" $ hello
