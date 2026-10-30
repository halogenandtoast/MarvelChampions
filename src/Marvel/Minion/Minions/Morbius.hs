module Marvel.Minion.Minions.Morbius where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Minion.Runner
import Marvel.Damage
import Marvel.Window qualified as W

morbius :: MinionCard Morbius
morbius = minion Morbius Cards.morbius (Sch 2) (Atk 2) (HP 6)

newtype Morbius = Morbius (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Morbius where
  getAbilities a@(Morbius attrs) =
    [ windowAbility a 1 (W.ResourcesGeneratedBy $ HeroIdentity <> IdentityWithId (minionEngagedIdentity attrs))
        ForcedResponse NoCost (runAbility a 1)
    ]

instance RunMessage Morbius where
  runMessage msg e@(Morbius attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [n | W.GeneratedResources you n <- windows, you == minionEngagedIdentity attrs] \n ->
        whenM (identityMatches HeroIdentity $ minionEngagedIdentity attrs) $
          damageIdentity (minionEngagedIdentity attrs) attrs (toDamage n FromAbility)
      pure e
    _ -> Morbius <$> runMessage msg attrs
