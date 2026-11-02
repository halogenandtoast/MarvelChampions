{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.SideScheme.SideSchemes.JessicaLuke where
import Marvel.Prelude
import Control.Monad (replicateM_)
import Data.Maybe (maybeToList)
import Data.HashMap.Strict qualified as HashMap
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
import Marvel.Keyword qualified as K
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
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.JessicaJones.Helpers
import Marvel.Obligation.Types (Obligation, Field (ObligationCardDef))
import Marvel.Phase
import Marvel.SideScheme.Types

newtype JLSideScheme = JLSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme JLSideScheme where
  sideSchemeAbilities = getAbilities

jl61007 :: SideSchemeCard JLSideScheme
jl61007 = sideScheme JLSideScheme Cards.jl61007 (Static 6)

jl61020 :: SideSchemeCard JLSideScheme
jl61020 = sideScheme JLSideScheme Cards.jl61020 (PerPlayer 4)

jl61028 :: SideSchemeCard JLSideScheme
jl61028 = sideScheme JLSideScheme Cards.jl61028 (PerPlayer 3)

jl61029 :: SideSchemeCard JLSideScheme
jl61029 = sideScheme JLSideScheme Cards.jl61029 (PerPlayer 3)

jl61032 :: SideSchemeCard JLSideScheme
jl61032 = sideScheme JLSideScheme Cards.jl61032 (PerPlayer 2)

jl61039 :: SideSchemeCard JLSideScheme
jl61039 = sideScheme JLSideScheme Cards.jl61039 (PerPlayer 3)

jl62030 :: SideSchemeCard JLSideScheme
jl62030 = sideScheme JLSideScheme Cards.jl62030 (PerPlayer 3)

instance HasModifiersFor JLSideScheme where
  getModifiersFor _ target (JLSideScheme a) = case toCardCode a of
    "61007" -> pure [NotCountedAgainstPlayerSideSchemeLimit | target == toRef a]
    "62030" -> do
      minions <- selectList (MinionWithTrait T.SerpentSociety)
      pure [CannotBeAttacked | target `elem` map MinionRef minions]
    _ -> pure []

instance HasAbilities JLSideScheme

instance RunMessage JLSideScheme where
  runMessage msg e@(JLSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a, toCardCode a == "61032" -> do
      suggestions <- suggestionObligations
      traverse_ (\oid -> placeCounters (ObligationRef oid) 2) suggestions
      JLSideScheme <$> runMessage msg a
    ObligationMessage oid (RevealObligation _) | toCardCode a == "61032" -> do
      def <- (\g -> abilityCardDef g $ ObligationRef oid) <$> getGame
      when (maybe False ((`elem` map cdCardCode suggestionDefs) . cdCardCode) def) $ placeCounters (ObligationRef oid) 2
      pure e
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      players <- getPlayers
      case toCardCode a of
        "61007" -> getDefeatedBy a >>= (`drawCards` 3)
        "61020" -> do
          skipNextVillainPhase
          traverse_ dealEncounterCard players
          you <- getDefeatedBy a
          withTheVillain you $ \v -> untilNextVillainPhase a v [CannotTakeDamage]
        "61028" -> for_ players $ \you -> chooseOne you
          [labeled "$label.secondChanceShuffle" $ do
            cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches HeroSpecificCard)
            shuffleIntoDeck you cards
          ,labeled "$label.done" (pure ())]
        "61029" -> do
          for_ players $ \you -> chooseOne you
            [labeled "$label.searchDeck" $ search you (SearchIdentityDeckAndDiscardPile you) CardWithTeamUp SearchDrawOne ShuffleBackIn
            ,labeled "$label.done" (pure ())]
          for_ players $ \you -> lastingModifiers a (IdentityEntity $ IdentityWithId you) [ResourceCostReductionFor CardWithTeamUp 1] False
        "61039" -> do
          for_ players $ \you -> do
            canFlip <- identityMatches (HeroIdentity <> IdentityCanChangeForm) you
            when canFlip $ chooseOne you [labeled "$label.changeToB" (changeToAlterEgo you),labeled "$label.done" (pure ())]
          cardSpecific a "healAlterEgos" players
        "62030" -> do
          you <- getDefeatedBy a
          cardSpecific a "findSerpent" you
        _ -> pure ()
      JLSideScheme <$> runMessage msg a
    CardSpecific (isTarget a -> True) "healAlterEgos" (fromJSON -> Success (players :: [IdentityId])) -> do
      for_ players $ \you -> whenM (identityMatches AlterEgoIdentity you) $ heal you =<< alterEgoRecovery you
      pure e
    CardSpecific (isTarget a -> True) "findSerpent" (fromJSON -> Success (you :: IdentityId)) -> do
      discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait T.SerpentSociety) a
      cardSpecific a "serpentPlayer" you
      pure e
    WithDiscardedMatch (isTarget a -> True) _ (EncounterCard card) | toCardCode a == "62030" -> do
      you <- getDefeatedBy a
      revealEncounterCardFrom W.RevealedFromDiscardPile you card
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "62030" -> do
      getBoostedPlayer (toEnemy enemy) >>= confuseIdentity
      pure e
    _ -> JLSideScheme <$> runMessage msg a

suggestionDefs :: [CardDef]
suggestionDefs = [Cards.jl61033a, Cards.jl61033b, Cards.jl61033c]

suggestionObligations :: HasGame m => m [ObligationId]
suggestionObligations = do
  players <- getPlayers
  obligations <- concat <$> traverse getObligationsOf players
  filterM (\oid -> (\g -> maybe False ((`elem` map cdCardCode suggestionDefs) . cdCardCode) $ abilityCardDef g $ ObligationRef oid) <$> getGame) obligations
