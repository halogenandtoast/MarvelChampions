-- | The minions of Adam Warlock's nemesis set.
module Marvel.Minion.Minions.AdamWarlockNemesis (
  theMagus,
  TheMagus (..),
  zealotOfTruth,
  ZealotOfTruth (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Window

-- | Quickstrike and toughness are its keywords.
theMagus :: MinionCard TheMagus
theMagus = minion TheMagus Cards.theMagus (Sch 2) (Atk 2) (HP 5)

newtype TheMagus = TheMagus (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Response: After The Magus activates against you (attacks you, or
-- schemes while engaged with you), discard the top 5 cards of your deck.
instance HasAbilities TheMagus where
  getAbilities (TheMagus a) =
    [ windowAbility a 1 (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity) ForcedResponse NoCost
        $ RunAbility (toTarget a) 1
    , windowAbility a 2 (MinionHasSchemed After (MinionWithId $ minionId a)) ForcedResponse NoCost
        $ RunAbility (toTarget a) 2
    ]

instance RunMessage TheMagus where
  runMessage msg e@(TheMagus attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttack _ you] _ -> do
      discardTopOfDeck you 5
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      discardTopOfDeck (minionEngagedIdentity attrs) 5
      pure e
    _ -> TheMagus <$> runMessage msg attrs

zealotOfTruth :: MinionCard ZealotOfTruth
zealotOfTruth = minion ZealotOfTruth Cards.zealotOfTruth (Sch 1) (Atk 2) (HP 4)

newtype ZealotOfTruth = ZealotOfTruth (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Threat cannot be removed from the Universal Church of Truth side scheme.
instance HasModifiersFor ZealotOfTruth where
  getModifiersFor _ (SideSchemeRef sid) _ = do
    church <- inAnyArea $ member sid <$> select (SideSchemeIs SideSchemes.universalChurchOfTruth)
    pure [ThreatCannotBeRemoved | church]
  getModifiersFor _ _ _ = pure []

-- | [star] Boost: Put Zealot of Truth into play engaged with you.
instance RunMessage ZealotOfTruth where
  runMessage msg e@(ZealotOfTruth attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      e <$ putBoostIntoPlay target you
    _ -> ZealotOfTruth <$> runMessage msg attrs
