module Marvel.Ally.Allies.SilkCindyMoon where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card ()
import Marvel.Cost
import Marvel.Entity ()
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query ()
import Marvel.Trait (Trait (WebWarrior))
import Marvel.Window qualified as W

silkCindyMoon :: AllyCard SilkCindyMoon
silkCindyMoon = ally SilkCindyMoon Cards.silkCindyMoon (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype SilkCindyMoon = SilkCindyMoon (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SilkCindyMoon where
  getAbilities a@(SilkCindyMoon attrs) =
    [ limitedWindowAbility a 1 (W.PlayedFromHand You $ CardIs Cards.silkCindyMoon) Response
        (OwnsThis <> AnyCriteria
          [ SelfMatches (IdentityWithTrait WebWarrior)
          , AllyExists (AllyControlledBy You <> AllyWithTrait WebWarrior <> NotAlly (AllyWithId $ allyId attrs))
          , SupportExists (SupportMatches [SupportControlledBy You, SupportWithTrait WebWarrior])
          , UpgradeExists (upgradeControlledBy (allyController attrs) <> UpgradeWithTrait WebWarrior)
          ]) NoCost (runAbility a 1)
    ]

instance RunMessage SilkCindyMoon where
  runMessage msg a@(SilkCindyMoon attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      deck <- getEncounterDeck
      let treacheries = filter (cardMatch $ CardWithType TreacheryType) deck
      unless (null treacheries) $
        chooseOneSearchedIn you "$label.searchEncounterDeck" [("$label.encounterDeck", map EncounterCard deck)]
          [targeting card (discardCard card) | card <- treacheries]
      shuffleEncounterDeck
      pure a
    _ -> SilkCindyMoon <$> runMessage msg attrs
