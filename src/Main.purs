module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Portal (class PortalM, portalM)
import Halogen.VDom.Driver (runUI)
import Type.Prelude (Proxy(..))

data Action = Toggle

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall q i o m. MonadAff m => PortalM m => H.Component q i o m
component = H.mkComponent
  { initialState: const { open: true }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where

  handleAction Toggle = do
    liftEffect $ log "Toggle"
    H.modify_ \st -> st { open = not st.open }

  render { open } = HH.div_
    [ HH.text $ "Modal is " <> (if open then "open" else "closed")
    , HH.br_
    , HH.button
        [ HE.onClick $ const Toggle ]
        [ HH.text $ if open then "Close" else "Open" ]
    , if open then
        portalM (Proxy @"child") unit child unit Nothing identity
      else HH.text ""
    ]

child :: forall q i m. H.Component q i Action m
child = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = H.raise }
  }
  where

  render _ = HH.div_
    [ HH.text "I am the modal"
    , HH.br_
    , HH.button
        [ HE.onClick $ const Toggle ]
        [ HH.text "Close" ]
    ]
