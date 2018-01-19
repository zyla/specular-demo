module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Monoid (mempty)
import Specular.Dom.Builder.Class (dynText, el, text)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnChange)
import Specular.FRP (Dynamic, current, dynamic, dynamic_, holdDyn, leftmost, pull, weaken)
import Specular.FRP.Base (foldDyn, readBehavior)

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody mainWidget

data What = Button | TextField

mainWidget
  :: forall m
   . MonadWidget m
  => m Unit
mainWidget = do
  clickedBtn <- buttonOnClick (pure mempty) (text "Button")
  clickedText <- buttonOnClick (pure mempty) (text "Text field")

  (value :: Dynamic What) <- holdDyn Button $
    leftmost
      [ Button    <$ clickedBtn
      , TextField <$ clickedText
      ]

  el "br" (pure unit)

  someOtherValue <- textInputOnChange "foo" mempty

  el "br" (pure unit)

  let
    dynWidget :: Dynamic (m (Dynamic String))
    dynWidget =
      (\what -> case what of

         Button -> do
           void $ buttonOnClick (pure mempty) (text "Btn")
           pure (pure "It's a button!")

         TextField -> do
           initial <- pull $ readBehavior $ current someOtherValue
           textInputOnChange initial mempty

      ) <$> value

  value2 <- map join $ dynamic dynWidget

  el "br" (pure unit)

  dynText $ weaken value2

-- dynamic ∷ ∀ m a. MonadWidget m ⇒ Dynamic (m a) → m (Dynamic a)
