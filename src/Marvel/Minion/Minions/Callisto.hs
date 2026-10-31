module Marvel.Minion.Minions.Callisto where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Stats
import Marvel.Treachery.Cards qualified as Treacheries

callisto :: MinionCard Callisto
callisto = minion Callisto Cards.callisto (Sch 1) (Atk 3) (HP 5)

newtype Callisto = Callisto (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Callisto where
  getAbilities a@(Callisto attrs) =
    [ limitedWindowAbility a 1
        (W.EncounterCardRevealedBy W.When AnyIdentity $ EncounterCardMatching $ CardIs Treacheries.knifeFight)
        ForcedInterrupt (CharacterExists $ CharacterWithId (MinionCharacter $ minionId attrs) <> CharacterCanGainTough) NoCost (runAbility a 1)
    ]

instance RunMessage Callisto where
  runMessage msg c@(Callisto attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> c <$ giveStatus (minionId attrs) ToughStatus
    _ -> Callisto <$> runMessage msg attrs
