module Marvel.SideScheme.SideSchemes.MojoGenres where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Window qualified as W

newtype MojoGenreSideScheme = MojoGenreSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39037, mojo39038, mojo39039, mojo39045, mojo39059 :: SideSchemeCard MojoGenreSideScheme
mojo39037 = sideScheme MojoGenreSideScheme Cards.mojo39037 (Static 5)
mojo39038 = sideScheme MojoGenreSideScheme Cards.mojo39038 (Static 5)
mojo39039 = sideScheme MojoGenreSideScheme Cards.mojo39039 (Static 5)
mojo39045 = sideScheme MojoGenreSideScheme Cards.mojo39045 (PerPlayer 6)
mojo39059 = sideScheme MojoGenreSideScheme Cards.mojo39059 (PerPlayer 2)

instance IsSideScheme MojoGenreSideScheme where
  sideSchemeAbilities (MojoGenreSideScheme a) =
    [ability a 1 HeroAction NoCriteria (DynamicResourceCost $ Just resource) (runAbility a 1) | Just resource <- [case toCardCode a of "39037" -> Just Mental; "39038" -> Just Energy; "39039" -> Just Physical; _ -> Nothing]]
    <> [windowAbility a 2 W.RoundEnds ForcedInterrupt NoCost (runAbility a 2) | toCardCode a == "39059"]

instance HasModifiersFor MojoGenreSideScheme where
  getModifiersFor _ target (MojoGenreSideScheme a) = pure $ case toCardCode a of
    "39037" | target /= toRef a, isScheme target -> [ThreatCannotBeRemoved]
    "39038" | isFriendly target -> [AttackPenalty 2]
    "39039" | VillainRef _ <- target -> [CannotTakeDamage]
    "39059" | MinionRef _ <- target -> [KeywordModifier Guard, KeywordModifier Patrol]
    _ -> []
   where
    isScheme = \case MainSchemeRef _ -> True; SideSchemeRef _ -> True; _ -> False
    isFriendly = \case IdentityRef _ -> True; AllyRef _ -> True; _ -> False

instance RunMessage MojoGenreSideScheme where
  runMessage msg e@(MojoGenreSideScheme attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      removeThreat you attrs (paymentX payment) (SchemeWithId $ SchemeSideSchemeId $ sideSchemeId attrs)
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      first <- selectJust FirstPlayer
      searchAndPutIntoPlayEncounterCard first (CardWithType MinionType)
      pure e
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs, toCardCode attrs == "39045" -> do
      players <- getPlayers
      for_ players $ cardSpecific attrs "fetchQuest"
      MojoGenreSideScheme <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "fetchQuest" (fromJSON -> Success you) -> do
      cards <- unDeck <$> project PlayerIdentityDeck you
      legal <- asYou you $ filterM (canPlayCardIgnoringResourceCostFromAnyZone you) cards
      unless (null legal) $ chooseOne you
        (labeled "$label.done" (pure ()) : [targeting card (addToHand you card >> playCardIgnoringResourceCost you card) | card <- legal])
      shuffleDeck you
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "39045" -> do
      you <- getBoostedPlayer enemy
      putBoostIntoPlay attrs you
      pure e
    _ -> MojoGenreSideScheme <$> runMessage msg attrs
