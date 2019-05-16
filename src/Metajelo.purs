module Metajelo where

import Prelude                              (Unit, bind, pure, ($), (<>))

import Concur.React.Run                     (runWidgetInDom)
import Concur.React.DOM                     (div, span, text)
import Concur.React.Props                   (className)
import Data.Unfoldable1                     (singleton)
import Effect                               (Effect)
import Effect.Exception                     as EX
import Effect.Exception                     (Error)
import Metajelo.View                        (mjCssPfx, mkRecordWidget)
import Metajelo.XPaths                      as MXP

-- | Convenience method for the most typical use: reading in a
-- | a Metajelo XML doc string (`xmlStr`) and rendering the view
-- | in an element with id `elemId`.
renderRecord :: String -> String -> Effect Unit
renderRecord elemId xmlStr = EX.catchException displayError do
  parseEnv <- MXP.getDefaultParseEnv xmlStr
  record <- MXP.readRecord parseEnv
  recWidg <- pure $ mkRecordWidget record
  runWidgetInDom elemId recWidg
  where
    displayError :: Error -> Effect Unit
    displayError er = runWidgetInDom elemId $
      div  [className $ mjCssPfx "errorDisplayBox"] $ singleton $
        span [className $ mjCssPfx "errorDisplay"]
          [text $ (EX.name er) <> ": " <> (EX.message er)]
