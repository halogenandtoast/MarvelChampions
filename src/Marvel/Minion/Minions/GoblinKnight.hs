module Marvel.Minion.Minions.GoblinKnight (
  goblinKnight,
  GoblinKnight (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Trait (Trait (Goblin))
import Marvel.Window

goblinKnight :: MinionCard GoblinKnight
goblinKnight = minion GoblinKnight Cards.goblinKnight (Sch 2) (Atk 2) (HP 7)

newtype GoblinKnight = GoblinKnight (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GoblinKnight where
  getAbilities (GoblinKnight a) =
    [ windowAbility a 1 (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) You) ForcedResponse NoCost $
        runAbility a 1
    ]

instance RunMessage GoblinKnight where
  runMessage msg e@(GoblinKnight attrs) = runQueueT $ case msg of
    -- discard 1 encounter card: a Goblin minion is put into play engaged with you
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      discarded <- take 1 <$> getEncounterDeck
      discardTopOfEncounterDeck 1
      for_ (filter isGoblinMinion discarded) (putEncounterCardIntoPlay you)
      pure e
    -- Boost: after this activation ends, shuffle this card into the encounter deck
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      afterMatching (== ClearBoosts) $ shuffleIntoEncounterDeck [MkEncounterCard (CardId $ unMinionId $ minionId attrs) (getCardDef attrs)]
      pure e
    _ -> GoblinKnight <$> runMessage msg attrs

isGoblinMinion :: EncounterCard -> Bool
isGoblinMinion card = cdCardType (getCardDef card) == MinionType && Goblin `member` cdTraits (getCardDef card)
