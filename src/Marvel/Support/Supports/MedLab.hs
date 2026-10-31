module Marvel.Support.Supports.MedLab where

import Marvel.Question (runAbility)
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source (canPlayHeldAlly, canCaptureDefeatedAlly)
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Projection
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

medLab :: SupportCard MedLab
medLab = support MedLab Cards.medLab

newtype MedLab = MedLab (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MedLab where
  getAbilities a@(MedLab attrs) =
    [limitedWindowAbility a 1 (W.AllyDefeatedByConsequentialDamage W.After AnyIdentity)
      Response (OwnsThis <> MedLabCaptureAvailable) ExhaustCost (runAbility a 1) | null (supportHeldCards attrs)] <>
    [ability a 2 AlterEgoAction (OwnsThis <> HeldAllyPlayable (supportId attrs)) ExhaustCost (runAbility a 2)]

instance RunMessage MedLab where
  runMessage msg a@(MedLab attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      when (null $ supportHeldCards attrs) $ for_ [card | W.AllyCardDefeatedByConsequentialDamage _ card <- windows] $ \card ->
        whenM (canCaptureDefeatedAlly card) $ attachAllyToMedLab (supportId attrs) card
      pure a
    CardSpecific (isTarget attrs -> True) "holdAlly" (fromJSON -> Success card) ->
      pure $ MedLab attrs {supportHeldCards = [card]}
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      cards <- filterM (canPlayHeldAlly you) (supportHeldCards attrs)
      chooseOne you [targeting card (playHeldAllyExhausted you (supportId attrs) card) | card <- cards]
      pure a
    _ -> MedLab <$> runMessage msg attrs
