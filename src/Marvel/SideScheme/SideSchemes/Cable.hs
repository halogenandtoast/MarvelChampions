module Marvel.SideScheme.SideSchemes.Cable where

import Marvel.Hero.Cards qualified as HeroCards
import Marvel.Prelude
import Data.List (delete)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Status
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword
import {-# SOURCE #-} Marvel.Game (getPlayerVictoryDisplay, getVictoryDisplay, sideSchemesInVictoryDisplay)

import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.GameValue
import Marvel.Hand
import Marvel.Deck
import Marvel.Discard
import Marvel.Identity.Types
import Marvel.Projection

cable40006 :: SideSchemeCard Cable40006
cable40006 = sideScheme Cable40006 Cards.cable40006 (PerPlayer 5)

newtype Cable40006 = Cable40006 (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40006 where
  runMessage msg (Cable40006 attrs) = runQueueT $ Cable40006 <$> runMessage msg attrs

cable40018 :: SideSchemeCard Cable40018
cable40018 = sideScheme Cable40018 Cards.cable40018 (PerPlayer 3)

newtype Cable40018 = Cable40018 (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40018 where
  runMessage msg a@(Cable40018 attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \you -> chooseOne you
        [ labeled "$label.searchDeck" $ cardSpecific attrs "searchEligible" you
        , labeled "$label.done" (pure ())
        ]
      pure a
    CardSpecific (isTarget attrs -> True) "searchEligible" (fromJSON -> Success you) -> do
      searchEligibleCards attrs you (CardWithType AllyType)
      pure a
    CardSpecific (isTarget attrs -> True) "putSearchedCard" (fromJSON -> Success (you, card)) -> do
      putSearchedCard you card
      pure a
    _ -> Cable40018 <$> runMessage msg attrs

cable40019 :: SideSchemeCard Cable40019
cable40019 = sideScheme Cable40019 Cards.cable40019 (PerPlayer 2)

newtype Cable40019 = Cable40019 (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40019 where
  runMessage msg a@(Cable40019 attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \you -> chooseOne you
        [ labeled "$label.searchDeck" $ cardSpecific attrs "searchEligible" you
        , labeled "$label.done" (pure ())
        ]
      pure a
    CardSpecific (isTarget attrs -> True) "searchEligible" (fromJSON -> Success you) -> do
      searchEligibleCards attrs you (CardWithType UpgradeType <> CardWithTrait Trait.Weapon <> CardWithPrintedCostAtMost 3)
      pure a
    CardSpecific (isTarget attrs -> True) "putSearchedCard" (fromJSON -> Success (you, card)) -> do
      putSearchedCard you card
      pure a
    _ -> Cable40019 <$> runMessage msg attrs

cable40020 :: SideSchemeCard Cable40020
cable40020 = sideScheme Cable40020 Cards.cable40020 (PerPlayer 2)

newtype Cable40020 = Cable40020 (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40020 where
  runMessage msg a@(Cable40020 attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players (`giveStatus` ToughStatus)
      pure a
    _ -> Cable40020 <$> runMessage msg attrs

cable40027 :: SideSchemeCard Cable40027
cable40027 = sideScheme Cable40027 Cards.cable40027 (PerPlayer 3)

newtype Cable40027 = Cable40027 (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40027 where
  runMessage msg a@(Cable40027 attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \you -> chooseOne you
        [ labeled "$label.searchDeck" $ cardSpecific attrs "searchEligible" you
        , labeled "$label.done" (pure ())
        ]
      pure a
    CardSpecific (isTarget attrs -> True) "searchEligible" (fromJSON -> Success you) -> do
      searchEligibleCards attrs you (CardWithType SupportType <> CardWithPrintedCostAtMost 3)
      pure a
    CardSpecific (isTarget attrs -> True) "putSearchedCard" (fromJSON -> Success (you, card)) -> do
      putSearchedCard you card
      pure a
    _ -> Cable40027 <$> runMessage msg attrs

cable40033 :: SideSchemeCard Cable40033
cable40033 = sideScheme Cable40033 Cards.cable40033 (PerPlayer 4)

newtype Cable40033 = Cable40033 (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40033 where
  runMessage msg (Cable40033 attrs) = runQueueT $ Cable40033 <$> runMessage msg attrs

instance HasModifiersFor Cable40006 where
  getModifiersFor _ target (Cable40006 attrs) | target == toRef attrs = do
    cables <- selectList (IdentityIs HeroCards.cable40001a)
    pure [ThreatRemovableOnlyByCharacter $ IdentityCharacter you | you <- cables]
  getModifiersFor _ _ _ = pure []

instance HasModifiersFor Cable40033 where
  getModifiersFor _ target (Cable40033 attrs) = do
    cables <- selectList (IdentityIs HeroCards.cable40001a)
    case headMay cables of
      Nothing -> pure []
      Just you -> pure $ case target of
        IdentityRef p | p == you -> [CanOnlyRemoveThreatFrom $ SchemeSideSchemeId $ sideSchemeId attrs, CanOnlyDamageEngagedEnemies]
        IdentityRef _ -> [CannotRemoveThreatFrom $ SchemeSideSchemeId $ sideSchemeId attrs, CannotDamageEnemiesEngagedWith you]
        _ -> []

-- Search effects put cards into play, respecting uniqueness and attachment
-- requirements while ignoring the restrictions that apply only to playing them.
searchEligibleCards :: (ReverseQueue m, IsRef source) => source -> IdentityId -> CardMatcher -> m ()
searchEligibleCards source you matcher = do
  deck <- projectMap PlayerIdentityDeck unDeck you
  discard <- projectMap PlayerIdentityDiscard unDiscard you
  allowed <- selectList (BasicCardMatches matcher <> CanEnterPlay)
  cards <- filterM (fmap (maybe True notNull) . getPlayTargets you) (filter (`elem` allowed) (deck <> discard))
  focusing (PlayerCard <$> cards) $ chooseOne you $
    [targeting card $ cardSpecific source "putSearchedCard" (you, card) | card <- cards]
    <> [labeled "$label.done" (pure ())]
  shuffleDeck you

putSearchedCard :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
putSearchedCard you card = do
  targets <- getPlayTargets you card
  case targets of
    Nothing -> putIntoPlay you card
    Just choices -> chooseOrRunOne you [targeting target $ putIntoPlayOn you card target | target <- choices]
