module Metajelo where

import Prelude

import Concur.React.Run                     (runWidgetInDom)
import Effect                               (Effect)
import Metajelo.View                        (mkRecordWidget)
import Metajelo.XPaths                      as MXP

-- | Convenience method for the most typical use: reading in a
-- | a Metajelo XML doc string (`xmlStr`) and rendering the view
-- | in an element with id `elemId`.
renderRecord :: String -> String -> Effect Unit
renderRecord elemId xmlStr = do
  parseEnv <- MXP.getDefaultParseEnv xmlStr
  record <- MXP.readRecord parseEnv
  recWidg <- pure $ mkRecordWidget record
  runWidgetInDom elemId recWidg
