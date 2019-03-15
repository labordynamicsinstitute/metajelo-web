module Main where

import Prelude

import Concur.Core                          (Widget, andd)
import Concur.Core.FRP                      (Signal, display, dyn, hold)
import Concur.React                         (HTML)
import Concur.React.DOM                     (button, button', div_, div',
                                             input, p', text)
import Concur.React.Props                   (_type, checked, onChange,
                                             onChecked, onClick, onFocus,
                                             unsafeTargetValue, value)
import Concur.React.Run                     (runWidgetInDom)
import Control.Alt                          ((<|>))
import Control.MultiAlternative             (orr)
import Data.Maybe                           (fromMaybe)
import Data.Traversable                     (traverse)
import Effect                               (Effect)
import Effect.Class                         (liftEffect)
import Effect.Class.Console                 (log)
import React.SyntheticEvent                 (SyntheticMouseEvent)

hello :: forall a. Widget HTML a
hello = do
  greeting :: String <- orr
    [ "Hello" <$ button [onClick] [text "Say Hello"]
    , "Namaste" <$ button [onClick] [text "Say Namaste"]
    ]
  text (greeting <> " Sailor!")

helloWidget :: forall a. Widget HTML a
helloWidget = do
  greeting :: String <- orr
    [ "Hello" <$ button [onClick] [text "Say Hello"]
    , "Namaste" <$ button [onClick] [text "Say Namaste"]
    ]
  liftEffect $ log ("You chose to say " <> greeting)
  text (greeting <> " Sailor!")

buttonHello :: Widget HTML SyntheticMouseEvent
buttonHello = button [onClick] [text "Say Hello"]

data Action = Changed String | Focused
inputWidget1 :: Widget HTML Action
inputWidget1 = input [(Changed <<< unsafeTargetValue) <$> onChange,
                      Focused <$ onFocus]

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

-- A counter widget takes the initial count as argument,
-- and returns the updated count
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

listCounters3 :: Array Int -> Widget HTML (Array Int)
listCounters3 initialCounts = andd (map counter initialCounts)


{- *** Not working currently
-- This is like Elm's State
type Form =
  { name :: String
  , rememberMe :: Boolean
  }

-- This is like Elm's Action
data FormAction
  = Name String
  | RememberMe Boolean
  | Submit

formWidget :: Form -> Widget HTML Form
formWidget form = do
  -- This is like Elm's view function
  res <- div'
    [ Name <$> input [ _type "text", value form.name,
                       unsafeTargetValue <$> onChange]
    , RememberMe <$> input [_type "checkbox", checked form.rememberMe,
                            onChecked]
    , Submit <$ button [value "Submit", onClick]
    ]
  -- This is like Elm's update function
  case res of
    Name s -> formWidget (form {name = s})
    RememberMe b -> formWidget (form {rememberMe = b})
    Submit -> pure form
-}



getGreeting :: Widget HTML String
getGreeting = div'
    [ "Hello" <$ button [onClick] [text "Say Hello"]
    , "Namaste" <$ button [onClick] [text "Say Namaste"]
    ]

showGreeting :: String -> Widget HTML Unit
showGreeting greeting = div'
  [ text (greeting <> " Sailor!")
  , void $ button [onClick] [text "restart"]
  ]

helloGreets :: forall a. Widget HTML a
helloGreets = do
  greeting <- getGreeting
  showGreeting greeting
  helloGreets

helloComp :: String -> forall a. Widget HTML a
helloComp prev = helloComp =<< div'
  [ text ("Previous greeting - " <> prev)
  , do
      greeting <- getGreeting
      showGreeting greeting
      pure greeting
  ]

{- FIXME: not working. understand type of display
helloSig :: String -> Signal HTML String
helloSig s = display $ helloComp s


helloListWithDisplay :: Array String -> Signal HTML (Array String)
helloListWithDisplay prev = div_ [] do
  traverse helloComp prev
  display (text ("Previously selected greetings - " <> show prev))
-}

main :: Effect Unit
main = runWidgetInDom "root" $ focusCountWidget
-- main = runWidgetInDom "root" $ listCounters3 [1, 1, 2, 3, 5]
-- main = runWidgetInDom "root" helloListWithDisplay
