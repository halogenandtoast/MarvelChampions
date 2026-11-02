module Marvel.SideScheme.SideSchemes.TricksterTakeover where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.TricksterTakeover.Helpers
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.SideScheme.Types

newtype TTSideScheme = TTSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme TTSideScheme where
  sideSchemeAbilities = getAbilities

tt55006 :: SideSchemeCard TTSideScheme
tt55006 = sideScheme TTSideScheme Cards.tt55006 (PerPlayer 2)

tt55021 :: SideSchemeCard TTSideScheme
tt55021 = sideScheme TTSideScheme Cards.tt55021 (Static 2)

tt55022 :: SideSchemeCard TTSideScheme
tt55022 = sideScheme (\a -> TTSideScheme a {sideSchemeCrisis = True}) Cards.tt55022 (Static 4)

tt55045 :: SideSchemeCard TTSideScheme
tt55045 = sideScheme (\a -> TTSideScheme a {sideSchemeCrisis = True}) Cards.tt55045 (Static 4)

tt55046 :: SideSchemeCard TTSideScheme
tt55046 = sideScheme (\a -> TTSideScheme a {sideSchemeCrisis = True}) Cards.tt55046 (Static 7)

tt55047 :: SideSchemeCard TTSideScheme
tt55047 = sideScheme TTSideScheme Cards.tt55047 (Static 5)

tt55048 :: SideSchemeCard TTSideScheme
tt55048 = sideScheme TTSideScheme Cards.tt55048 (Static 6)

tt55060 :: SideSchemeCard TTSideScheme
tt55060 = sideScheme TTSideScheme Cards.tt55060 (Static 4)

tt55061 :: SideSchemeCard TTSideScheme
tt55061 = sideScheme TTSideScheme Cards.tt55061 (Static 2)

instance HasModifiersFor TTSideScheme where
  getModifiersFor _ target (TTSideScheme a) = do
    villain <- selectOne ActiveVillain
    allies <- selectList AnyAlly
    hard <- expert
    pure $ case toCardCode a of
      "55006" | Just target == (VillainRef <$> villain) -> [KeywordModifier Stalwart, CannotTakeDamage]
      "55060" | Just target == (VillainRef <$> villain) -> [KeywordModifier Stalwart]
      "55061" | target `elem` map AllyRef allies -> [CannotDefendAgainstVillain, CannotThwartScheme $ SchemeSideSchemeId $ sideSchemeId a]
      "55061" | target == toRef a, hard -> [KeywordModifier $ Hinder $ PerPlayer 1]
      _ -> []

instance HasAbilities TTSideScheme where
  getAbilities (TTSideScheme a) =
    [windowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId $ sideSchemeId a) WhenRevealed NoCost (runAbility a 1) | toCardCode a `elem` ["55046", "55047", "55048"]]

instance RunMessage TTSideScheme where
  runMessage msg e@(TTSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid SideSchemePlaceInitialThreat | sid == sideSchemeId a, toCardCode a == "55061" -> do
      hard <- expert
      count <- getPlayerCount
      pure $ TTSideScheme a {sideSchemeThreat = 2 + if hard then fromIntegral count else 0}
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      you <- getYou
      case toCardCode a of
        "55006" -> getAllPlayers >>= traverse_ charm
        "55021" -> discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait Trait.Enthralled) a
        "55022" -> getAllPlayers >>= traverse_ (\p -> do
          enslaved <- enthralled p
          if enslaved then discardControlledMatching p AnyCard else charm p)
        "55045" -> do
          hard <- expert
          discardTopOfEncounterDeckThen (if hard then 3 else 2) $ \cards -> when (any isTreachery cards) $ exhaust you
        _ -> pure ()
      pure e
    WithDiscardedMatch (isTarget a -> True) FromEncounterDeck (EncounterCard card) | toCardCode a == "55021" -> do
      you <- getYou
      revealFoundEncounterCard you card
      pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      case toCardCode a of
        "55046" -> shatter 3 >> chooseSynergy you 1
        "55047" -> shatter 2
        "55048" -> shatter 3 >> synergy "55053" 1
        _ -> pure ()
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "55021" -> charm you
        "55047" -> selectList (CharacterControlledBy $ IdentityWithId you) >>= dealIndirectDamage you a . fromIntegral . length
        "55060" -> attackGains enemy Overkill
        _ -> pure ()
      pure e
    _ -> TTSideScheme <$> runMessage msg a
