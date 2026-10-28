-- | The minions of the Black Order and Armies of Titan modular sets (The Mad Titan's
-- Shadow).
module Marvel.Minion.Minions.BlackOrder (
  blackDwarf,
  BlackDwarf (..),
  supergiant,
  Supergiant (..),
  blackOrderInfantry,
  BlackOrderInfantry (..),
  outrider,
  Outrider (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window hiding (MinionDefeated)

blackDwarf :: MinionCard BlackDwarf
blackDwarf = minion BlackDwarf Cards.blackDwarf (Sch 1) (Atk 3) (HP 6)

newtype BlackDwarf = BlackDwarf (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Black Dwarf's attacks gain overkill.
instance HasModifiersFor BlackDwarf where
  getModifiersFor _ (MinionRef mid) (BlackDwarf a) | mid == minionId a = pure [KeywordModifier Overkill]
  getModifiersFor _ _ _ = pure []

instance RunMessage BlackDwarf where
  runMessage msg (BlackDwarf attrs) = BlackDwarf <$> runMessage msg attrs

supergiant :: MinionCard Supergiant
supergiant = minion Supergiant Cards.supergiant (Sch 2) (Atk 2) (HP 5)

newtype Supergiant = Supergiant (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Quickstrike. [star] Forced Response: after Supergiant attacks and damages a
-- character, that character is stunned.
instance HasAbilities Supergiant where
  getAbilities (Supergiant a) =
    [ windowAbility a 1 (EnemyAttackedAndDamaged (EnemyWithId $ EnemyMinionId $ minionId a) AnyCharacter) ForcedResponse NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage Supergiant where
  runMessage msg e@(Supergiant attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttacksAndDamages _ character _] _ -> do
      giveStatus character StunnedStatus
      pure e
    _ -> Supergiant <$> runMessage msg attrs

blackOrderInfantry :: MinionCard BlackOrderInfantry
blackOrderInfantry = minion BlackOrderInfantry Cards.blackOrderInfantry (Sch 1) (Atk 2) (HP 4)

newtype BlackOrderInfantry = BlackOrderInfantry (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Guard. When Defeated: give the villain a tough status card. [star] Boost: the same.
instance RunMessage BlackOrderInfantry where
  runMessage msg e@(BlackOrderInfantry attrs) = runQueueT $ case msg of
    MinionMessage ident MinionDefeated | ident == minionId attrs -> do
      toughVillain
      BlackOrderInfantry <$> runMessage msg attrs
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      toughVillain
      pure e
    _ -> BlackOrderInfantry <$> runMessage msg attrs
   where
    toughVillain = traverse_ enemyBecomesTough =<< selectOne ActiveVillain

outrider :: MinionCard Outrider
outrider = minion Outrider Cards.outrider (Sch 1) (Atk 1) (HP 2)

newtype Outrider = Outrider (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: discard 1 card at random from your hand. [star] Boost: the same.
instance RunMessage Outrider where
  runMessage msg e@(Outrider attrs) = runQueueT $ case msg of
    MinionMessage ident (RevealMinion you) | ident == minionId attrs -> do
      discardFrom you RandomFromHand 1
      Outrider <$> runMessage msg attrs
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      discardFrom you RandomFromHand 1
      pure e
    _ -> Outrider <$> runMessage msg attrs
