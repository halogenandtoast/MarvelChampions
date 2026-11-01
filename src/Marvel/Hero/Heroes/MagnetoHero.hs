module Marvel.Hero.Heroes.MagnetoHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.Identity.Types (Field(..))

import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner

magnetoHero :: HeroCard MagnetoHero
magnetoHero = hero MagnetoHero Cards.magnetoHero (HP $ Static 10) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

newtype MagnetoHero = MagnetoHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MagnetoHero where
  getAbilities a = [label "$label.magneticPull" $ limitedAbility a 1 (PerRound 1) Action
    (IsSelf <> ExtendedCardExists (InDeckOf You $ BasicCardMatches AnyCard)) NoCost (runAbility a 1)]

instance RunMessage MagnetoHero where
  runMessage msg e@(MagnetoHero attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ cardSpecific attrs "pull" ([] :: [PlayerCard])
    CardSpecific (isTarget attrs -> True) "pull" (fromJSON -> Success (cards :: [PlayerCard])) -> do
      deck <- projectMap PlayerIdentityDeck unDeck (heroIdentityId attrs)
      case deck of
        [] -> cardSpecific attrs "pullFailed" cards
        top : rest -> do
          discardFromDeck (heroIdentityId attrs) 1 attrs
          cardSpecific attrs "pullNext" (cards, top, null rest)
      pure e
    CardSpecific (isTarget attrs -> True) "pullNext" (fromJSON -> Success (cards :: [PlayerCard], top :: PlayerCard, lastCard :: Bool)) -> do
      let real = isNothing (playerZoneEncounterCard top)
          discarded = cards <> [top | real]
          magnetic = real && cardMatch (CardWithTrait T.Magnetic) top
      if magnetic then addToHand (heroIdentityId attrs) top >> cardSpecific attrs "pulled" discarded
        else cardSpecific attrs (if lastCard && real then "pullFailed" else "pull") discarded
      pure e
    CardSpecific (isTarget attrs -> True) "pullFailed" (fromJSON -> Success cards) -> do
      checkWindows [W.Window W.After $ W.MagneticPullFailed (heroIdentityId attrs) cards]
      pure e
    CardSpecific (isTarget attrs -> True) "pulled" (fromJSON -> Success cards) -> do
      checkWindows [W.Window W.After $ W.MagneticPullResolved (heroIdentityId attrs) cards]
      pure e
    _ -> MagnetoHero <$> runMessage msg attrs
