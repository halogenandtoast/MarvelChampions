module Marvel.MainScheme.MainSchemes.NextEvolutionNeStryfe where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers
import Marvel.NextEvolution.Stryfe
import Marvel.Phase
import Marvel.Projection
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NeStryfeMainScheme = NeStryfeMainScheme (Attrs MainScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40166 :: MainSchemeCard NeStryfeMainScheme
ne40166 = mainScheme NeStryfeMainScheme Cards.ne40166 (PerPlayer 9) (Static 0) (PerPlayer 0)

ne40167 :: MainSchemeCard NeStryfeMainScheme
ne40167 = mainScheme NeStryfeMainScheme Cards.ne40167 (PerPlayer 8) (Static 0) (PerPlayer 1)

instance IsMainScheme NeStryfeMainScheme where
  mainSchemeModifiersFor _ target (NeStryfeMainScheme a) | toCardCode a == "40167" = pure $ case target of
    IdentityRef _ -> [HandSizeModifier 2, AdditionalResourceCost AnyCard 1]
    VillainRef _ -> [KeywordModifier Stalwart]
    _ -> []
  mainSchemeModifiersFor _ _ _ = pure []

instance RunMessage NeStryfeMainScheme where
  runMessage msg e@(NeStryfeMainScheme a) = runQueueT $ case msg of
    SetupMainScheme | toCardCode a == "40166" -> do
      -- Hope's Setup keyword brings the physical encounter card into play.
      first <- selectJust FirstPlayer
      revealEncounterCard first =<< genEncounterCard SideSchemes.ne40168a
      pure e
    VillainPhaseStep StepEnemiesActivate | toCardCode a == "40166" -> do
      players <- getPlayers
      for_ players $ \you -> cardSpecific a "neStryfeThreatChoice" you
      pure e
    CardSpecific target "neStryfeThreatChoice" (fromJSON -> Success you) | target == toRef a -> do
      cards <- unHand <$> project PlayerIdentityHand you
      unless (null cards) $ chooseOne you $
        [labeled "$label.done" (pure ())] <>
        [targeting card (discardCard card) | card <- cards]
      cardSpecific a "neStryfeThreatCount" you
      pure e
    CardSpecific target "neStryfeThreatCount" (fromJSON -> Success you) | target == toRef a -> do
      n <- handTypeCount you
      placeThreat (mainSchemeId a) n
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a -> e <$ gameOver Lost
    _ -> NeStryfeMainScheme <$> runMessage msg a
