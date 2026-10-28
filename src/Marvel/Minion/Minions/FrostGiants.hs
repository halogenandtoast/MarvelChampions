-- | The minions of the Frost Giants modular set (The Mad Titan's Shadow); its Frost
-- Giant is the card of Thor's nemesis set ('Marvel.Minion.Minions.FrostGiant').
module Marvel.Minion.Minions.FrostGiants (
  laufey,
  Laufey (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window hiding (MinionDefeated)

laufey :: MinionCard Laufey
laufey = minion Laufey Cards.laufey (Sch 2) (Atk 4) (HP 6)

newtype Laufey = Laufey (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Toughness. [star] Forced Response: after Laufey attacks and damages a character,
-- stun that character.
instance HasAbilities Laufey where
  getAbilities (Laufey a) =
    [ windowAbility a 1 (EnemyAttackedAndDamaged (EnemyWithId $ EnemyMinionId $ minionId a) AnyCharacter) ForcedResponse NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage Laufey where
  runMessage msg e@(Laufey attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttacksAndDamages _ character _] _ -> do
      giveStatus character StunnedStatus
      pure e
    _ -> Laufey <$> runMessage msg attrs
