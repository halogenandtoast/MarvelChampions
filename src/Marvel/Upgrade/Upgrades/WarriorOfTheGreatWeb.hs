module Marvel.Upgrade.Upgrades.WarriorOfTheGreatWeb where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Question (runAbility)
import Marvel.Game.Source
import Marvel.Name
import Marvel.Effect.Types
import Marvel.Trait
import Data.Text qualified as Text

warriorOfTheGreatWeb :: UpgradeCard WarriorOfTheGreatWeb
warriorOfTheGreatWeb = upgrade WarriorOfTheGreatWeb Cards.warriorOfTheGreatWeb

newtype WarriorOfTheGreatWeb = WarriorOfTheGreatWeb (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade WarriorOfTheGreatWeb where
  upgradeTargets _ = do
    players <- getPlayers
    identities <- filterM (\p -> do
      named <- hasSpiderTitle (toTarget p)
      open <- selectNone (UpgradeIs Cards.warriorOfTheGreatWeb <> UpgradeOnIdentity (IdentityWithId p))
      pure $ named && open) players
    allies <- selectList (NotAlly $ AllyWithUpgrade $ UpgradeIs Cards.warriorOfTheGreatWeb)
    namedAllies <- filterM (hasSpiderTitle . toTarget) allies
    pure $ Just $ map toTarget identities <> map toTarget namedAllies
   where
    hasSpiderTitle target = getsGame $ \g -> maybe False (Text.isInfixOf "Spider" . title . cdName) (abilityCardDef g target)

host :: Attrs Upgrade -> Target
host attrs = maybe (toTarget $ upgradeController attrs) toTarget (upgradeAttachedAlly attrs)

instance HasModifiersFor WarriorOfTheGreatWeb where
  getModifiersFor _ target (WarriorOfTheGreatWeb attrs) = pure [TraitModifier WebWarrior | target == host attrs]

instance HasAbilities WarriorOfTheGreatWeb where
  getAbilities a = [limitedWindowAbility a 1 (W.AllyLeavesPlay W.After AnyIdentity $ CardWithTrait WebWarrior)
    Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage WarriorOfTheGreatWeb where
  runMessage msg a@(WarriorOfTheGreatWeb attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let matcher = maybe (IdentityEntity $ IdentityWithId $ upgradeController attrs) (AllyEntity . AllyWithId) (upgradeAttachedAlly attrs)
      createEffect Cards.warriorOfTheGreatWeb attrs matcher
      pure a
    _ -> WarriorOfTheGreatWeb <$> runMessage msg attrs

newtype WarriorOfTheGreatWebEffect = WarriorOfTheGreatWebEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

warriorOfTheGreatWebEffect :: CardEffect WarriorOfTheGreatWebEffect
warriorOfTheGreatWebEffect = effectWith WarriorOfTheGreatWebEffect Cards.warriorOfTheGreatWeb
  ((modifiersL .~ [AttackModifier 1]) . (endsL ?~ DisableAtEndOfPhase))

instance RunMessage WarriorOfTheGreatWebEffect where
  runMessage msg (WarriorOfTheGreatWebEffect attrs) = WarriorOfTheGreatWebEffect <$> runMessage msg attrs
