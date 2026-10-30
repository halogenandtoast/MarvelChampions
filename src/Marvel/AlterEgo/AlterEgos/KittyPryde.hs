module Marvel.AlterEgo.AlterEgos.KittyPryde where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Upgrade.Cards qualified as Upgrades

kittyPryde :: AlterEgoCard KittyPryde
kittyPryde = alterEgo KittyPryde Cards.kittyPryde (HP $ Static 9) (HandSize 6) (Rec 3) [Obligations.permanentlyPhased]

newtype KittyPryde = KittyPryde (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities KittyPryde where
  getAbilities (KittyPryde a) =
    [ ability a 1 Setup IsSelf NoCost (runAbility a 1)
    , limitedAbility a 2 (PerRound 1) Action (IsSelf <> SelfMatches IdentityCanChangeMassForm) NoCost (runAbility a 2)
    ]

instance RunMessage KittyPryde where
  runMessage msg e@(KittyPryde attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> e <$ putMassFormIntoPlay you
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      forms <- selectList (UpgradeIs Upgrades.solid <> UpgradeControlledBy (IdentityWithId you))
      phased <- selectList (UpgradeIs Upgrades.phased <> UpgradeControlledBy (IdentityWithId you))
      for_ (headMay $ forms <> phased) (flipMassForm you)
      pure e
    _ -> KittyPryde <$> runMessage msg attrs
