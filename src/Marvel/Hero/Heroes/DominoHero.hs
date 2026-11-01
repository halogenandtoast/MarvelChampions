module Marvel.Hero.Heroes.DominoHero where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner

dominoHero :: HeroCard DominoHero
dominoHero = hero DominoHero Cards.dominoHero (HP $ Static 9) (HandSize 5) (Thw 1) (Atk 2) (Def 3)
newtype DominoHero = DominoHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DominoHero where
  getAbilities a = [limitedAbility a 1 (PerRound 1) Action
    (IsSelf <> ExtendedCardExists (InHandOf You (BasicCardMatches AnyCard)) <> ExtendedCardExists (InDeckOf You (BasicCardMatches AnyCard)))
    NoCost (runAbility a 1)]

instance RunMessage DominoHero where
  runMessage msg a@(DominoHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      hand <- projectMap PlayerIdentityHand unHand you
      chooseOne you [targeting card (cardSpecific you "swapHandWithDeckTop" card) | card <- hand]
      pure a
    _ -> DominoHero <$> runMessage msg attrs
