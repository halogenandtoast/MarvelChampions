module Marvel.Ally.Allies.ManifoldShuri where
import Marvel.Prelude
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Discard ()
import Marvel.Deck ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Ally.Runner

manifoldShuri :: AllyCard ManifoldShuri
manifoldShuri = ally ManifoldShuri Cards.manifoldShuri (Thw 2, 1) (Atk 1, 1) (HP 2)
newtype ManifoldShuri = ManifoldShuri (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage ManifoldShuri where
  runMessage msg u@(ManifoldShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- selectList AnyIdentity
      chooseOne you [targeting player (search player (SearchIdentityDeckAndDiscardPile player) (CardWithType PlayerSideSchemeType) SearchDrawOne ShuffleBackIn) | player <- players]
      pure u
    _ -> ManifoldShuri <$> runMessage msg attrs

instance HasAbilities ManifoldShuri where
  getAbilities u@(ManifoldShuri _attrs) = [limitedWindowAbility u 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility u 1)]
