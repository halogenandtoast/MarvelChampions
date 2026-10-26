module Marvel.Treachery.Treacheries.Intimidation (
  intimidation,
  Intimidation (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

intimidation :: TreacheryCard Intimidation
intimidation = treachery Intimidation Cards.intimidation

newtype Intimidation = Intimidation (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Spend 2 resources of any type or give the villain a facedown boost card, turned up
-- with its next activation's.
instance RunMessage Intimidation where
  runMessage msg t@(Intimidation attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      -- with no villain in play (as between one Kang and the next), nothing
      selectOne ActiveVillain >>= \case
        Nothing -> pure t
        Just villain -> do
          let cost = MultiResourceCost [Nothing, Nothing]
          canPay <- canAffordCost you (toSource attrs) cost
          chooseOrRunOne you $
            labeled "$label.giveTheVillainAFacedownBoostCard" (dealBoost villain)
              : [labeled (countLabel "spendResources" 2) (payForTreachery you cost) | canPay]
          pure t
    -- [star] Boost: the villain is given another boost card for this activation; a
    -- minion's activation is not the villain's
    Boost (RevealedAsBoost target (EnemyVillainId vid)) | isTarget attrs target -> do
      dealBoost vid
      pure t
    _ -> Intimidation <$> runMessage msg attrs
