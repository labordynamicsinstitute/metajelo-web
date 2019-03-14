module Main where

import Prelude

import Concur.Core                          (Widget)
import Concur.React                         (HTML)
import Concur.React.DOM                     (button, button', div', input, p', text)
import Concur.React.Props                   (onChange, onClick, onFocus, unsafeTargetValue)
import Concur.React.Run                     (runWidgetInDom)
import Control.Alt                          ((<|>))
import Control.MultiAlternative             (orr)
import Data.Maybe                           (fromMaybe)
import Effect                               (Effect)
import React.SyntheticEvent                 (SyntheticMouseEvent)

hello :: forall a. Widget HTML a
hello = do
  greeting <- orr
    [ "Hello" <$ button [onClick] [text "Say Hello"]
    , "Namaste" <$ button [onClick] [text "Say Namaste"]
    ]
  text (greeting <> " Sailor!")

buttonHello :: Widget HTML SyntheticMouseEvent
buttonHello = button [onClick] [text "Say Hello"]

data Action = Changed String | Focused
inputWidget1 :: Widget HTML Action
inputWidget1 = input [(Changed <<< unsafeTargetValue) <$> onChange, Focused <$ onFocus]

type IWState = {focusCount:: Int, currentText :: String}
inputWidget2 :: IWState -> Widget HTML IWState
inputWidget2 st = input
  [ st {focusCount = st.focusCount+1} <$ onFocus
  , ((\s -> st {currentText = s}) <<< unsafeTargetValue) <$> onChange]

showState :: IWState -> String
showState s = "The current value of the input is "
              <> show s.currentText <> ",\n and you have focused it "
              <> show s.focusCount <> " times."

initState :: IWState
initState = {focusCount: 0, currentText: ""}

focusCountWidget :: forall a. Widget HTML a
focusCountWidget = go initState
  where
    go s = div'
      [ p' [text $ showState s]
      , inputWidget2 s
      ] >>= go

-- A counter widget takes the initial count as argument, and returns the updated count
counter :: Int -> Widget HTML Int
counter count = do
  void $ button [onClick] [text (show count)]
  pure (count + 1)

-- Compose a list of counters in parallel
-- The return value is the value of the counter which is clicked
listCounters :: Array Int -> Widget HTML Int
listCounters = orr <<< map counter


-- listCounters2 :: Array Int -> Widget HTML (Array Int)
-- listCounters2 initialCounts = orr (mapWithIndex (mkCount initialCounts) initialCounts)
--   where mkCount initialCountArray index initCount = map (\count -> fromMaybe initialCountArray (updateAt index count initialCountArray)) (counter initCount)

-- listCounters3 :: Array Int -> Widget HTML (Array Int)
-- listCounters3 initialCounts = andd (map counter initialCounts)


main :: Effect Unit
main = runWidgetInDom "root" $ focusCountWidget -- inputWidget2 $ {focusCount: 0, currentText: ""}
--main = runWidgetInDom "root" $ listCounters [1, 1, 2, 3, 5]
