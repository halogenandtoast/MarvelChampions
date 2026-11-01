module Marvel.AlterEgo.AlterEgos.LucasBishop where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Question
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations

lucasBishop :: AlterEgoCard LucasBishop
lucasBishop = alterEgo LucasBishop Cards.lucasBishop (HP $ Static 12) (HandSize 6) (Rec 4) [Obligations.fearTheFuture]

newtype LucasBishop = LucasBishop (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LucasBishop where
  getAbilities a@(LucasBishop attrs) =
    [label "$label.temporallyDisplaced" $ limitedWindowAbility a 1
      (W.IdentityChangedToForm W.After $ IdentityWithId $ alterEgoIdentityId attrs)
      Response (IsSelf <> ExtendedCardExists (InDiscardOf You $ BasicCardMatches $ CardWithTrait Trait.Temporal)) NoCost (runAbility a 1)]

instance RunMessage LucasBishop where
  runMessage msg a@(LucasBishop attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch $ CardWithTrait Trait.Temporal) <$> projectMap PlayerIdentityDiscard unDiscard you
      chooseOne you [targeting card $ addToHand you card | card <- cards]
      pure a
    _ -> LucasBishop <$> runMessage msg attrs
