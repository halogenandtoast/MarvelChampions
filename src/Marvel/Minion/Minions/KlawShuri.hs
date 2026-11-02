module Marvel.Minion.Minions.KlawShuri where
import Marvel.Prelude
import Marvel.Id
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Minion.Runner

klawShuri :: MinionCard KlawShuri
klawShuri = minion KlawShuri Cards.klawShuri (Sch 2) (Atk 0) (HP 6)
newtype KlawShuri = KlawShuri (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage KlawShuri where
  runMessage msg u@(KlawShuri attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> u <$ dealBoost attrs
    _ -> KlawShuri <$> runMessage msg attrs

instance HasAbilities KlawShuri where
  getAbilities u@(KlawShuri attrs) = [limitedWindowAbility u 1 (W.EnemyAttacked W.When (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity) ForcedInterrupt NoCriteria NoCost (runAbility u 1)]
