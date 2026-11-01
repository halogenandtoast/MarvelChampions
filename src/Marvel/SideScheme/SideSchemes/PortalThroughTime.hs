module Marvel.SideScheme.SideSchemes.PortalThroughTime where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

portalThroughTime :: SideSchemeCard PortalThroughTime
portalThroughTime = sideScheme PortalThroughTime Cards.portalThroughTime (Static 4)

newtype PortalThroughTime = PortalThroughTime (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme PortalThroughTime where
  sideSchemeAbilities a = [limitedAbility a 1 (PerPhase 1) ForcedInterrupt NoCriteria NoCost (runAbility a 1)
    & windowL ?~ W.EncounterCardRevealedBy W.When AnyIdentity (EncounterCardMatching $ CardWithTrait Trait.Temporal)]

instance RunMessage PortalThroughTime where
  runMessage msg a@(PortalThroughTime attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(you, card) | W.EncounterCardRevealed _ you card <- windows] $ \(you, card) -> giveRevealedCardSurge you card
      pure a
    _ -> PortalThroughTime <$> runMessage msg attrs
