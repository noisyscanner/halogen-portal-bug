module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Portal (portalAff)
import Halogen.VDom.Driver (runUI)
import Type.Prelude (Proxy(..))

data Action = Toggle | Increment | Query

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: const { open: true, counter: 0 }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where

  handleAction Toggle = do
    liftEffect $ log "Toggle"
    H.modify_ \st -> st { open = not st.open }

  handleAction Increment = H.modify_ \st -> st { counter = st.counter + 1 }
  handleAction Query = do 
  
    res <- H.request (Proxy @"child") unit Q2

    traceM {res}

  render { open, counter } = HH.div_
    [ HH.text $ "Modal is " <> (if open then "open" else "closed")
    , HH.br_
    , HH.button
        [ HE.onClick $ const Toggle ]
        [ HH.text $ if open then "Close" else "Open" ]
    , HH.button
        [ HE.onClick $ const Increment ]
        [ HH.text "Increment" ]
    , HH.button
        [ HE.onClick $ const Query ]
        [ HH.text "Query" ]
    , if open then
        portalAff (Proxy @"child") unit child { counter } Nothing identity
      else HH.text ""
    ]

type Input = { counter :: Int }

data ActionInt act = Raise act | Receive Input
data QueryInt a = BradsQuery a | Q2 (Int -> a)

child :: forall m. MonadAff m => H.Component QueryInt Input Action m
child = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where

  handleAction = case _ of
    Raise act -> do
      pure unit
      H.raise act
      pure unit
    Receive input -> H.put input

  handleQuery :: forall a. QueryInt a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery = case _ of
    BradsQuery a -> do
      pure $ Just a

    Q2 f -> do 
      pure $ Just $ f 42

  render { counter } = HH.div_
    [ HH.text "I am the modal"
    , HH.br_
    , HH.p_ [ HH.text $ show counter ]
    , HH.button
        [ HE.onClick $ const $ Raise Toggle ]
        [ HH.text "Close" ]
    ]
