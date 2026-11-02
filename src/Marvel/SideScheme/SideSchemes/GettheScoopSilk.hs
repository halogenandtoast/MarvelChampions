module Marvel.SideScheme.SideSchemes.GettheScoopSilk where
import Marvel.Prelude
import Marvel.Id
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

gettheScoopSilk :: SideSchemeCard GettheScoopSilk
gettheScoopSilk = sideScheme GettheScoopSilk Cards.gettheScoopSilk (Static 4)
newtype GettheScoopSilk = GettheScoopSilk (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage GettheScoopSilk where
  runMessage msg u@(GettheScoopSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      thwartSchemeAs you (sideSchemeId attrs) attrs 2
      pure u
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- selectList (IdentityIs Cards.cindyMoon)
      for_ (headMay players) $ \you -> cardSpecific attrs "look" you
      pure u
    CardSpecific (isTarget attrs -> True) "look" (fromJSON -> Success (you :: IdentityId)) -> do
      cards <- take 2 <$> getEncounterDeck
      lookingAt you (map EncounterCard cards) $ chooseOne you [targeting card (tuckEncounterCard you card) | card <- cards]
      pure u
    _ -> GettheScoopSilk <$> runMessage msg attrs

instance HasAbilities GettheScoopSilk where
  getAbilities u@(GettheScoopSilk _attrs) = [ability u 1 AlterEgoAction ForAnyPlayer (ExhaustCharacterCost $ IdentityCharacters You) (runAbility u 1)]

instance IsSideScheme GettheScoopSilk where
  toSideSchemeAttrs (GettheScoopSilk attrs) = attrs
  sideSchemeAbilities = getAbilities
