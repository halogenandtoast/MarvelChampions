module Marvel.Upgrade.Upgrades.LuckyBreak where
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
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

luckyBreak :: UpgradeCard LuckyBreak
luckyBreak = upgrade LuckyBreak Cards.luckyBreak
newtype LuckyBreak = LuckyBreak (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LuckyBreak where
  getAbilities a@(LuckyBreak attrs) = [limitedWindowAbility a 1
    (W.EncounterCardRevealedBy W.When (IdentityWithId $ upgradeController attrs) AnyEncounterCard)
    HeroInterrupt (OwnsThis <> CanCancelRevealedEncounterCard) (DiscardCost $ toTarget attrs) (runAbility a 1)]

instance RunMessage LuckyBreak where
  runMessage msg a@(LuckyBreak attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.EncounterCardRevealed _ _ card <- windows] $ \card -> do
        cancelEncounterCard card
        drawAndRevealEncounterCard you
      pure a
    _ -> LuckyBreak <$> runMessage msg attrs
