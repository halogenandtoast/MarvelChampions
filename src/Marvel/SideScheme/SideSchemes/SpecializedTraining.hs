module Marvel.SideScheme.SideSchemes.SpecializedTraining where

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
import Marvel.Window qualified as W
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue
import Marvel.Trait qualified as Trait

specializedTraining :: SideSchemeCard SpecializedTraining
specializedTraining = sideScheme SpecializedTraining Cards.specializedTraining (PerPlayer 5)

newtype SpecializedTraining = SpecializedTraining (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

availableSpecialists :: (ReverseQueue m) => m [PlayerCard]
availableSpecialists = do
  aside <- getSetAsidePlayerCards
  allowed <- selectList (BasicCardMatches (CardWithTrait Trait.Specialization) <> CanEnterPlay)
  pure $ filter (`elem` allowed) aside

instance RunMessage SpecializedTraining where
  runMessage msg a@(SpecializedTraining attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      cardSpecific attrs "nextPlayer" players
      pure a
    -- Each player resolves separately; the first player chooses the order (RR Each Player).
    CardSpecific (isTarget attrs -> True) "nextPlayer" (fromJSON -> Success (remaining :: [IdentityId])) -> do
      eligible <- filterM (\you -> selectNone (UpgradeControlledBy (IdentityWithId you) <> UpgradeWithTrait Trait.Specialization)) remaining
      available <- availableSpecialists
      unless (null eligible || null available) do
        first <- selectJust FirstPlayer
        chooseOrRunOne first
          [ targeting you do
              cardSpecific attrs "specialize" you
              cardSpecific attrs "nextPlayer" (filter (/= you) eligible)
          | you <- eligible
          ]
      pure a
    CardSpecific (isTarget attrs -> True) "specialize" (fromJSON -> Success you) -> do
      hasSpecialization <- selectAny (UpgradeControlledBy (IdentityWithId you) <> UpgradeWithTrait Trait.Specialization)
      unless hasSpecialization do
        cards <- availableSpecialists
        unless (null cards) $ chooseOne you [targeting card (putIntoPlay you card) | card <- cards]
      pure a
    _ -> SpecializedTraining <$> runMessage msg attrs
