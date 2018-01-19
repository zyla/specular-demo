module Main where

import Prelude

import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.IO (IO(..))
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Monoid (mempty)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Specular.Dom.Builder.Class (dynText, el, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnChange, textInputOnInput)
import Specular.FRP (Dynamic, current, dynamic, dynamic_, fixFRP_, holdDyn, leftmost, pull, tagDyn, weaken)
import Specular.FRP.Async (RequestState(..), asyncRequest, performEvent)
import Specular.FRP.Base (foldDyn, readBehavior, subscribeEvent_)
import Specular.FRP.WeakDynamic (subscribeWeakDyn_)

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody mainWidget

data What = Button | TextField

validate :: String -> IO Boolean
validate str = do
  liftAff $ delay (Milliseconds 1300.0)
  pure true

save :: String -> IO Unit
save str = do
  liftEff $ log $ "Saving: " <> str
  liftAff $ delay (Milliseconds 1300.0)
  liftEff $ log $ "Saved."

mainWidget
  :: forall m
   . MonadWidget m
  => m Unit
mainWidget = do
  value <- textInputOnChange "" mempty

  (validationState :: Dynamic (RequestState Boolean)) <- asyncRequest $ map validate value

  {-
  dynText $ weaken $ map
    (case _ of
       NotRequested -> ""
       Loading -> "Loading..."
       Loaded true -> "valid" 
       Loaded false -> "invalid") validationState
       -}

  fixFRP_ $ \omega -> do
    let buttonAttrs =
          map (\e -> if e then mempty else "disabled" := "disabled") omega.enabled
    submitClicked <- buttonOnClick buttonAttrs $ text "Submit"


    -- control

    let submit = tagDyn value submitClicked

    saved <- performEvent $ map save submit

    (loading :: Dynamic Boolean) <- holdDyn false $
      leftmost
        [ true  <$ submit
        , false <$ saved
        ]

    let enabled = map not loading

    pure {enabled}

-- asyncRequest :: Dynamic (IO a) -> m (Dynamic (RequestState a))
