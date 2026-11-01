module Marvel.SideScheme.SideSchemes.HuntingTheSpiderTotems where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait (Trait (Inheritor, WebWarrior))
import Marvel.Window qualified as W

huntingTheSpiderTotems :: SideSchemeCard HuntingTheSpiderTotems
huntingTheSpiderTotems = sideScheme HuntingTheSpiderTotems Cards.huntingTheSpiderTotems (Static 6)

newtype HuntingTheSpiderTotems = HuntingTheSpiderTotems (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme HuntingTheSpiderTotems where
  sideSchemeAbilities a =
    [windowAbility a 1 (W.PhaseBegins W.When VillainPhase) ForcedInterrupt NoCost (runAbility a 1)]

instance RunMessage HuntingTheSpiderTotems where
  runMessage msg s@(HuntingTheSpiderTotems attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      discardTopOfEncounterDeckFor 3 attrs
      pure s
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      let inheritors = [card | EncounterCard card <- cards
                            , cardMatch (CardWithType MinionType <> CardWithTrait Inheritor) card]
      cardSpecific attrs "putIntoPlay" inheritors
      pure s
    CardSpecific (isTarget attrs -> True) "putIntoPlay" (fromJSON -> Success (cards :: [EncounterCard])) -> do
      case cards of
        [] -> pure ()
        card : rest -> do
          players <- selectList AnyIdentity
          webPlayers <- filterM (\p -> selectAny (CharacterControlledBy (IdentityWithId p) <> CharacterWithTrait WebWarrior)) players
          first <- selectJust FirstPlayer
          chooseOrRunOne first
            [ targeting player do
                putEncounterCardIntoPlay player card
                cardSpecific attrs "putIntoPlay" rest
            | player <- if null webPlayers then [first] else webPlayers ]
      pure s
    _ -> HuntingTheSpiderTotems <$> runMessage msg attrs
