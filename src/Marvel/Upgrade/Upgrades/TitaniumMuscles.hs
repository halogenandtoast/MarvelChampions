module Marvel.Upgrade.Upgrades.TitaniumMuscles where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Payment ()

titaniumMuscles :: UpgradeCard TitaniumMuscles
titaniumMuscles = upgrade TitaniumMuscles Cards.titaniumMuscles

newtype TitaniumMuscles = TitaniumMuscles (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TitaniumMuscles where
  getAbilities (TitaniumMuscles a) =
    [ability a 1 HeroResource (OwnsThis <> CharacterExists (CharacterWithId (IdentityCharacter $ upgradeController a) <> CharacterWithStatus ToughStatus)) ExhaustCost (Pay $ PhysicalResourcesForTough $ upgradeController a)]

instance HasModifiersFor TitaniumMuscles where
  getModifiersFor _ (IdentityRef you) (TitaniumMuscles a) = pure [AttackModifier 1 | you == upgradeController a]
  getModifiersFor _ _ _ = pure []

instance RunMessage TitaniumMuscles where
  runMessage msg (TitaniumMuscles attrs) = runQueueT $ case msg of
    _ -> TitaniumMuscles <$> runMessage msg attrs
