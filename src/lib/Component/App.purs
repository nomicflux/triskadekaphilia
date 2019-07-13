module Component.App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.String as String
import Effect.Aff (Aff)
import Halogen as Ha
import Halogen.Component as H
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import TDP.Note as N
import Web.Event.Event as E
import Web.HTML as W
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State = { note :: Maybe N.Note
             , tuning :: N.Tuning
             }

data Action =
  Init
  | HandleKey Ha.SubscriptionId KeyboardEvent

component :: forall q i o. H.Component HH.HTML q i o Aff
component  =
  H.mkComponent { initialState
                , render
                , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                                 , initialize = Just Init
                                                 }
                }

keyToNote :: String -> Maybe N.Note
keyToNote key
  | key == "1" = Just N.c
  | key == "2" = Just N.d
  | key == "3" = Just N.e
  | key == "4" = Just N.f
  | key == "5" = Just N.g
  | key == "6" = Just N.h
  | key == "7" = Just N.j
  | key == "8" = Just N.a
  | key == "9" = Just N.b
  | key == "0" = Just $ N.shiftTritaveUp N.c
  | otherwise = Nothing

swapTuning :: N.Tuning -> N.Tuning
swapTuning N.JustIntonation = N.EqualTemperment
swapTuning N.EqualTemperment = N.JustIntonation

initialState :: forall i. i -> State
initialState _ = { note : Nothing
                 , tuning : N.JustIntonation
                 }

render :: forall m. State -> Ha.ComponentHTML Action () m
render state =
  HH.div_ [ HH.p_ [ HH.text $ "Holding down " <>
                    (M.maybe "" noteString state.note) ]]
  where
    noteString note = show note <> " - " <> show (N.runNote state.tuning note 440.0)

handleAction :: forall o. Action -> Ha.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Init -> do
    document <- Ha.liftEffect $ Web.document =<< W.window
    Ha.subscribe' \sid ->
      ES.eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
  HandleKey sid ev -> do
    Ha.liftEffect $ E.preventDefault (KE.toEvent ev)
    let char = KE.key ev
    when (char == "Enter") do
      Ha.modify_ (\st -> st { tuning = swapTuning st.tuning })
    when (String.length char == 1) do
      Ha.modify_ (\st -> st { note = keyToNote char })
