module Marvel.Ally.Allies.IndraMagneto where

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

indraMagneto :: AllyCard IndraMagneto
indraMagneto = ally IndraMagneto Cards.indraMagneto (Thw 2, 1) (Atk 2, 1) (HP 2)

newtype IndraMagneto = IndraMagneto (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor IndraMagneto where
  getModifiersFor _ target (IndraMagneto a) | target == toRef a = do
    eligible <- identityMatches (IdentityWithTrait T.Mutant) (allyController a)
    xmen <- identityMatches (IdentityWithTrait T.XMen) (allyController a)
    pure $ if eligible || xmen then [HitPointModifier 2, NotCountedAgainstAllyLimit] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage IndraMagneto where
  runMessage msg (IndraMagneto attrs) = runQueueT $ case msg of
    _ -> IndraMagneto <$> runMessage msg attrs
