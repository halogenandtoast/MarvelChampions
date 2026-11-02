{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Obligation.Obligations.Fw53029 where
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
import Marvel.Obligation.Types

fw53029 :: ObligationCard Fw53029
fw53029 = obligation (Fw53029 . (`With` (3 :: Natural))) Cards.fw53029

newtype Fw53029 = Fw53029 (Attrs Obligation `With` Natural)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsObligation Fw53029 where
  toObligationAttrs (Fw53029 (attrs `With` _)) = attrs
  obligationDisplayCounters (Fw53029 (_ `With` n)) = n

instance HasAbilities Fw53029 where
  getAbilities a@(Fw53029 (attrs `With` n)) =
    [ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId owner) (ResourceCost Nothing) (runAbility a 1) | n > 0, owner <- maybeToList $ obligationOwner attrs]

instance RunMessage Fw53029 where
  runMessage msg a@(Fw53029 (attrs `With` n)) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> pure $ Fw53029 (attrs {obligationOwner = Just you} `With` n)
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      when (n == 1) $ do
        for_ [card | EncounterCard card <- [toCard attrs]] addCardToVictoryDisplay
        removeFromGame attrs
      pure $ Fw53029 (attrs `With` subtractNatural 1 n)
    _ -> Fw53029 . (`With` n) <$> runMessage msg attrs
