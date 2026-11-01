module Marvel.MainScheme.MainSchemes.NextEvolutionOnTheRun where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (Marauder))
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype OnTheRunMainScheme = OnTheRunMainScheme (Attrs MainScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40103 :: MainSchemeCard OnTheRunMainScheme
ne40103 = mainScheme OnTheRunMainScheme Cards.ne40103 (PerPlayer 8) (PerPlayer 1) (PerPlayer 1)

ne40104 :: MainSchemeCard OnTheRunMainScheme
ne40104 = mainScheme OnTheRunMainScheme Cards.ne40104 (PerPlayer 9) (PerPlayer 1) (PerPlayer 2)

instance IsMainScheme OnTheRunMainScheme where
  mainSchemeModifiersFor _ target (OnTheRunMainScheme a) = case target of
    MinionRef mid -> do
      marauder <- selectAny (MinionWithId mid <> MinionWithTrait Marauder)
      pure $ [KeywordModifier Steady | marauder] <> [KeywordModifier Guard | marauder, toCardCode a == "40104"]
    VillainRef _ | toCardCode a == "40104" -> do
      difficulty <- getDifficulty
      pure [KeywordModifier Steady | difficulty == Expert]
    _ -> pure []

instance RunMessage OnTheRunMainScheme where
  runMessage msg e@(OnTheRunMainScheme a) = runQueueT $ case msg of
    SetupMainScheme | toCardCode a == "40103" -> do
      first <- selectJust FirstPlayer
      putEncounterCardIntoPlay first =<< genEncounterCard Attachments.ne40105a
      pure e
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a -> do
      players <- getPlayers
      for_ players \you -> cardSpecific a "neSearchMarauder" (you, toCardCode a == "40104")
      when (toCardCode a == "40104") $ cardSpecific a "neToughMarauders" ()
      pure e
    CardSpecific target "neSearchMarauder" (fromJSON -> Success (you, withDiscard)) | target == toRef a -> do
      deck <- getEncounterDeck
      discard <- if withDiscard then getEncounterDiscard else pure []
      let candidates = filter (cardMatch $ CardWithType MinionType <> CardWithTrait Marauder) (deck <> discard)
      unless (null candidates) $ chooseOrRunOneSearched you (map EncounterCard $ deck <> discard)
        [targeting card (putEncounterCardIntoPlay you card) | card <- candidates]
      shuffleEncounterDeck
      pure e
    CardSpecific target "neToughMarauders" _ | target == toRef a -> do
      enemies <- selectList (EnemyWithTrait Marauder)
      traverse_ enemyBecomesTough enemies
      pure e
    -- Both printed stages lose when completed. Stage two is entered only by the
    -- uncancelable When Revealed on desperate Hope's Captor, not by threat.
    AdvanceMainScheme mid | mid == mainSchemeId a -> e <$ gameOver Lost
    _ -> OnTheRunMainScheme <$> runMessage msg a
