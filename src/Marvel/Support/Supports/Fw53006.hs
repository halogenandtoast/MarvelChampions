{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Support.Supports.Fw53006 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Support.Types

fw53006 :: SupportCard Fw53006
fw53006 = supportWith (Fw53006 . (`With` Nothing)) Cards.fw53006 ((usesL .~ 5) . (discardIfNoUsesL .~ True))

newtype Fw53006 = Fw53006 (Attrs Support `With` Maybe CardId)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSupport Fw53006 where
  toSupportAttrs (Fw53006 (attrs `With` _)) = attrs

instance HasAbilities Fw53006 where
  getAbilities a@(Fw53006 (_ `With` used)) =
    [(ability a 1 Resource (OwnsThis <> PayingFor (CardWithTrait T.Aerial) <> maybe NoCriteria PayingForCardOtherThan used) UseCost (runAbility a 1)) {abilityChoices = [runAbility a 1, Pay $ ResourcePayment R.Energy]}]

instance RunMessage Fw53006 where
  runMessage msg a@(Fw53006 (attrs `With` used)) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      card <- getActiveCost >>= maybe (pure Nothing) getActiveCostCard
      pure $ Fw53006 (attrs `With` (pcCardId <$> card))
    IdentityMessage _ (PlayedCard card _) | Just (pcCardId card) == used -> pure $ Fw53006 (attrs `With` Nothing)
    _ -> Fw53006 . (`With` used) <$> runMessage msg attrs
