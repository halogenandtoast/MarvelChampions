module Marvel.Treachery.Treacheries.WickedAmbitions (
  wickedAmbitions,
  WickedAmbitions (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Card
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Trait (Trait (Goblin))

wickedAmbitions :: TreacheryCard WickedAmbitions
wickedAmbitions = treachery WickedAmbitions Cards.wickedAmbitions

newtype WickedAmbitions = WickedAmbitions (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: discard twice the villain's stage from the encounter deck; for each
-- Goblin minion discarded, take 3 damage or put it into play engaged with you.
instance RunMessage WickedAmbitions where
  runMessage msg t@(WickedAmbitions attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      stage <- getVillainStage =<< selectJust ActiveVillain
      discarded <- take (fromIntegral $ 2 * stage) <$> getEncounterDeck
      discardTopOfEncounterDeck (2 * stage)
      for_ (filter isGoblinMinion discarded) \goblin ->
        chooseOne
          you
          [ labeled "$label.take3Damage" $ damageIdentity you attrs (toDamage 3 FromTreachery)
          , targeting goblin $ putEncounterCardIntoPlay you goblin
          ]
      pure t
    _ -> WickedAmbitions <$> runMessage msg attrs

isGoblinMinion :: EncounterCard -> Bool
isGoblinMinion card = cdCardType (getCardDef card) == MinionType && Goblin `member` cdTraits (getCardDef card)
