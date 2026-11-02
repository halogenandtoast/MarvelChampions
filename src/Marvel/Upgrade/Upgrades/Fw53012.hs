{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw53012 where
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

import Marvel.FalconWinter.Helpers
fw53012 :: UpgradeCard Fw53012
fw53012 = upgrade (Fw53012 . (`With` ([] :: [EncounterCard]))) Cards.fw53012

newtype Fw53012 = Fw53012 (Attrs Upgrade `With` [EncounterCard])
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade Fw53012 where
  toUpgradeAttrs (Fw53012 (attrs `With` _)) = attrs

instance HasAbilities Fw53012 where
  getAbilities a@(Fw53012 (attrs `With` _)) =
    [limitedWindowAbility a 1 (W.UsesAbility W.After (IdentityWithId $ upgradeController attrs) 1) HeroResponse (OwnsThis) (DiscardCost $ toTarget attrs) (runAbility a 1)]

instance RunMessage Fw53012 where
  runMessage msg a@(Fw53012 (attrs `With` icons)) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      countFalconIcons attrs "talon" you icons
      pure a
    WithBoostIcons (isTarget attrs -> True) value counts -> a <$ resolveFalconIcons attrs value counts
    CardSpecific (isTarget attrs -> True) "talon" (fromJSON -> Success (you, n :: Natural)) -> do
      when (n > 0) $ do
        characters <- selectList $ CharacterControlledBy (IdentityWithId you) <> CharacterCanBeReadied
        enemies <- selectList EnemyCanBeStunned
        let choices =
              [targeting (characterRef character) (readyFriendlyCharacter character >> cardSpecific attrs "talon" (you, n - 1)) | character <- characters]
              <> [targeting (enemyRef enemy) (stun enemy attrs >> cardSpecific attrs "talon" (you, n - 1)) | enemy <- enemies]
        unless (null choices) $ chooseOne you choices
      pure a
    WithDiscarded (IdentityRef who) FromEncounterDeck cards | who == upgradeController attrs -> pure $ Fw53012 (attrs `With` [card | EncounterCard card <- cards])
    _ -> Fw53012 . (`With` icons) <$> runMessage msg attrs
