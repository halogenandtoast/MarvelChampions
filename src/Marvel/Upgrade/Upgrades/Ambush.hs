module Marvel.Upgrade.Upgrades.Ambush where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

ambush :: UpgradeCard Ambush
ambush = upgrade Ambush Cards.ambush

newtype Ambush = Ambush (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade Ambush where
  upgradeTargets _ = do
    schemes <- selectList (NotSideScheme $ SideSchemeWithUpgrade $ UpgradeIs Cards.ambush)
    legal <- filterM (fmap (\modifiers -> CannotHaveAttachedCards `notElem` modifiers && CannotHaveAttachedPlayerCards `notElem` modifiers) . getModifiers . SideSchemeRef) schemes
    pure $ Just $ map SideSchemeRef legal

instance HasModifiersFor Ambush where
  getModifiersFor _ (UpgradeRef uid) (Ambush attrs) = pure [CrisisIcon | uid == upgradeId attrs]
  getModifiersFor _ _ _ = pure []

ambushTargets :: MinionMatcher
ambushTargets = NotMinion (MinionWithTrait Trait.Elite) <> MinionCanBeDiscarded

instance HasAbilities Ambush where
  getAbilities a@(Ambush attrs) =
    [ limitedWindowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId sid)
        Interrupt (OwnsThis <> MinionExists ambushTargets) NoCost (runAbility a 1)
    | Just sid <- [upgradeAttachedSideScheme attrs]
    ]

instance RunMessage Ambush where
  runMessage msg u@(Ambush attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      minions <- selectList ambushTargets
      unless (null minions) $ chooseOne you [targeting (enemyRef minion) (removeFromPlay minion) | minion <- minions]
      pure u
    _ -> Ambush <$> runMessage msg attrs
