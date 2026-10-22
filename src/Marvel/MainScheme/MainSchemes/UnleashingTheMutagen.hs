module Marvel.MainScheme.MainSchemes.UnleashingTheMutagen where

import Marvel.Prelude

import Data.List (find)
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Goblin))

unleashingTheMutagen :: MainSchemeCard UnleashingTheMutagen
unleashingTheMutagen =
  mainScheme UnleashingTheMutagen Cards.unleashingTheMutagen (PerPlayer 7) (PerPlayer 2) (PerPlayer 1)

newtype UnleashingTheMutagen = UnleashingTheMutagen (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

isGoblinMinion :: EncounterCard -> Bool
isGoblinMinion card = cdCardType def == MinionType && Goblin `member` cdTraits def
 where
  def = getCardDef card

instance RunMessage UnleashingTheMutagen where
  runMessage msg ms@(UnleashingTheMutagen attrs) = runQueueT $ case msg of
    -- 1A: a Goblin Thrall into play engaged with each player, from the encounter deck
    SetupMainScheme -> do
      players <- getPlayers
      deck <- getEncounterDeck
      let thralls = filter ((== Minions.goblinThrall) . getCardDef) deck
      for_ (zip players thralls) \(p, thrall) -> putEncounterCardIntoPlay p thrall
      shuffleEncounterDeck
      pure ms
    -- 1B, When Completed: in player order, each player not engaged with a Goblin minion
    -- discards 3 encounter cards and puts the first Goblin minion of them into play
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> do
      players <- getPlayers
      cardSpecific attrs "goblins" players
      nextMainScheme (mainSchemeId attrs)
      pure ms
    CardSpecific (isTarget attrs -> True) "goblins" (fromJSON -> Success (players :: [IdentityId])) -> case players of
      [] -> pure ms
      p : rest -> do
        engaged <- selectAny (MinionWithTrait Goblin <> MinionEngagedWith (IdentityWithId p))
        unless engaged do
          discarded <- take 3 <$> getEncounterDeck
          discardTopOfEncounterDeck 3
          for_ (find isGoblinMinion discarded) (putEncounterCardIntoPlay p)
        cardSpecific attrs "goblins" rest
        pure ms
    _ -> UnleashingTheMutagen <$> runMessage msg attrs
