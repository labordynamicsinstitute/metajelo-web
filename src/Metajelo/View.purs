module Metajel.View where

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
import Metajelo.Types
import React.SyntheticEvent                 (SyntheticMouseEvent)


mkRecordWidget :: MetajeloRecord -> forall a. Widget HTML a
mkRecordWidget record =
  div' [ text (recId <> " Sailor!") ]
  where
    recId = record.identifier.id

-- mkSupplementaryProductWidget :: SupplementaryProduct -> forall a. Widget HTML a
-- mkSupplementaryProductWidget (SupplementaryProduct recStr) =
--   div' [ text (recStr <> " Sailor!") ]

-- rec1 = MetajeloRecord "foo"

-- recArray :: Array Record
-- recArray = [rec1, rec2]

-- mkProdArrayWidg :: Array SupplementaryProduct ->  forall a. Widget HTML a
-- mkProdArrayWidg ra = div' $ map mkRecordWidget ra

