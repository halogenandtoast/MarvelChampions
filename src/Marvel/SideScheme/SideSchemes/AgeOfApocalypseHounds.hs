module Marvel.SideScheme.SideSchemes.AgeOfApocalypseHounds where

import Marvel.Prelude
import Marvel.Id
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability ()
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaHoundsSideScheme = AoaHoundsSideScheme (Attrs SideScheme `With` Maybe IdentityId)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

releaseTheHounds :: SideSchemeCard AoaHoundsSideScheme
releaseTheHounds = sideScheme (AoaHoundsSideScheme . (`With` Nothing)) Cards.releaseTheHounds (Static 5)

instance HasCardCode AoaHoundsSideScheme where
  toCardCode (AoaHoundsSideScheme (attrs `With` _)) = toCardCode attrs

instance IsRef AoaHoundsSideScheme where
  toRef (AoaHoundsSideScheme (attrs `With` _)) = toRef attrs

instance IsSideScheme AoaHoundsSideScheme where
  toSideSchemeAttrs (AoaHoundsSideScheme (attrs `With` _)) = attrs
  sideSchemeAbilities a = [limitedAbility a 1 (PerPhase 1) ForcedInterrupt NoCriteria NoCost (runAbility a 1)
    & windowL ?~ W.EncounterCardRevealedBy W.When AnyIdentity (EncounterCardMatching $ CardIs Minions.hound)]
instance RunMessage AoaHoundsSideScheme where
  runMessage msg a@(AoaHoundsSideScheme (attrs `With` defeatedBy)) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(you, card) | W.EncounterCardRevealed _ you card <- windows] $ \(you, card) -> giveRevealedCardSurge you card
      pure a
    SideSchemeMessage sid (SideSchemeThwarted source _) | sid == sideSchemeId attrs -> do
      you <- maybe getActivePlayerId pure =<< getSourceController source
      attrs' <- runMessage msg attrs
      pure $ AoaHoundsSideScheme $ attrs' `With` Just you
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- maybe getActivePlayerId pure defeatedBy
      searchAndDealEncounterCard you $ CardIs Minions.hound
      pure a
    _ -> AoaHoundsSideScheme . (`With` defeatedBy) <$> runMessage msg attrs
