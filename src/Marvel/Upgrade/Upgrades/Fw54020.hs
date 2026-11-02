{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54020 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Upgrade.Types

import Marvel.Keyword (Keyword(Restricted,Overkill))
import Marvel.FalconWinter.Helpers
import Marvel.Effect.Types
fw54020 :: UpgradeCard Fw54020
fw54020 = upgradeWith Fw54020 Cards.fw54020 ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype Fw54020 = Fw54020 (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54020 where
  getAbilities a@(Fw54020 attrs) =
    [limitedWindowAbility a 1 (W.WindowOneOf $
      [W.MakesBasicAttack W.When $ IdentityWithId you | you <- maybeToList $ upgradeAttachedIdentity attrs]
      <> [W.AllyAttacked W.When (AllyWithId aid) AnyEnemy | aid <- maybeToList $ upgradeAttachedAlly attrs]) Interrupt OwnsThis (ExhaustCost <> UseCost) (runAbility a 1)]

instance IsUpgrade Fw54020 where
  upgradeTargets _ = do
    characters <- selectList $ CharacterWithTrait T.Shield
    legal <- filterM (fmap not . hasUpgradeOnCharacter Cards.fw54020) characters
    pure $ Just $ map characterRef legal

instance RunMessage Fw54020 where
  runMessage msg a@(Fw54020 attrs) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade (Just (IdentityRef who))) | uid == upgradeId attrs -> do
      upgradeStep uid (UpgradeAttachedToIdentity who)
      Fw54020 <$> runMessage msg attrs
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      chooseDamage you attrs (toDamage 1 FromAbility) AnyEnemy
      pure a
    _ -> Fw54020 <$> runMessage msg attrs
