module Marvel.Ally.Allies.AnoleMagneto where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

anoleMagneto :: AllyCard AnoleMagneto
anoleMagneto = ally AnoleMagneto Cards.anoleMagneto (Thw 2, 1) (Atk 2, 1) (HP 2)

newtype AnoleMagneto = AnoleMagneto (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AnoleMagneto where
  getModifiersFor _ target (AnoleMagneto a) | target == toRef a = do
    eligible <- identityMatches (IdentityWithTrait T.Mutant) (allyController a)
    xmen <- identityMatches (IdentityWithTrait T.XMen) (allyController a)
    pure $ if eligible || xmen then [ThwartModifier 1, NotCountedAgainstAllyLimit] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage AnoleMagneto where
  runMessage msg (AnoleMagneto attrs) = runQueueT $ case msg of
    _ -> AnoleMagneto <$> runMessage msg attrs
