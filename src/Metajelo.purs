module Metajelo where

import Prelude                              (Unit, bind, pure, unit, ($), (<>))

import Concur.React.Run                     (runWidgetInDom)
import Concur.React.DOM                     (div, span, text)
import Concur.React.Props                   (className)
import Data.Unfoldable1                     (singleton)
import Effect                               (Effect)
import Effect.Exception                     as EX
import Effect.Exception                     (Error)
import Metajelo.CSS.Web.ClassProps          as MC
import Metajelo.View                        (mkRecordWidget)
import Metajelo.XPaths                      as MX
import Metajelo.XPaths.Read                 as MXR

-- | Convenience method for the most typical use: reading in a
-- | a Metajelo XML doc string (`xmlStr`) and rendering the view
-- | in an element with id `elemId`.
renderRecord :: String -> String -> Effect Unit
renderRecord elemId xmlStr = EX.catchException displayError do
  parseEnv <- MX.getDefaultParseEnv xmlStr
  record <- MXR.readRecord parseEnv
  recWidg <- pure $ mkRecordWidget record
  runWidgetInDom elemId recWidg
  where
    displayError :: Error -> Effect Unit
    displayError er = runWidgetInDom elemId $
      div [MC.errorDisplayBox] $ singleton $
        span [MC.errorDisplay]
          [text $ (EX.name er) <> ": " <> (EX.message er)]

main :: Effect Unit
main = pure unit
