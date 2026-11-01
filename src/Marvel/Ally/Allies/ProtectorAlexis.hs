module Marvel.Ally.Allies.ProtectorAlexis where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability ()
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Query ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait ()
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Upgrade.Cards qualified as Upgrades ()
import Marvel.Window qualified as W
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Resource (Resource (Mental))
protectorAlexis :: AllyCard ProtectorAlexis
protectorAlexis = ally ProtectorAlexis Cards.protectorAlexis (Thw 1, 1) (Atk 3, 1) (HP 3)

newtype ProtectorAlexis = ProtectorAlexis (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ProtectorAlexis where
  getAbilities (ProtectorAlexis a) =
    [ (limitedWindowAbility a 1 (W.AllyWouldTakeDamage $ AllyWithId $ allyId a) Interrupt OwnsThis (ResourceCost $ Just Mental) (runAbility a 1)) {abilityLimit = PerRound 1}
    ]

instance RunMessage ProtectorAlexis where
  runMessage msg a@(ProtectorAlexis attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      preventAllyDamage (allyId attrs) 1
      pure a
    _ -> ProtectorAlexis <$> runMessage msg attrs
