module Marvel.SideScheme.SideSchemes.TheInnerCircle where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source (countCardsInPlayWithTrait)
import Marvel.GameValue
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theInnerCircle :: SideSchemeCard TheInnerCircle
theInnerCircle = sideScheme TheInnerCircle Cards.theInnerCircle (Static 4)

newtype TheInnerCircle = TheInnerCircle (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TheInnerCircle where
  runMessage msg e@(TheInnerCircle attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      n <- countCardsInPlayWithTrait T.Hellfire
      placeThreatOn attrs (sideSchemeId attrs) (2 * n)
      pure e
    _ -> TheInnerCircle <$> runMessage msg attrs
