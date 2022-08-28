{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.ActiveCost
  ( module Marvel.ActiveCost
  , module Marvel.ActiveCost.Types
  ) where

import Marvel.Prelude

import Data.List (partition)
import Data.List qualified as L
import Marvel.ActiveCost.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Message
import Marvel.Payment
import Marvel.Projection
import Marvel.Identity.Types
import Marvel.Question
import Marvel.Queue
import Marvel.Resource

instance RunMessage ActiveCost where
  runMessage msg activeCost = case msg of
    CreatedActiveCost -> do
      cards <- getAvailablePaymentSources
      abilities <- getResourceAbilities
      paid <- resourceCostPaid activeCost
      push
        $ Ask (activeCostIdentityId activeCost)
        $ ChooseOne
        $ map PayWithCard cards
        <> map UseAbility abilities
        <> [ FinishPayment | paid ]
      pure activeCost
    Spent discard -> do
      case activeCostTarget activeCost of
        ForCard card -> do
          resources <- resourcesFor discard $ Just card
          push $ Paid $ mconcat $ map ResourcePayment resources
        ForAbility _ -> do
          resources <- resourcesFor discard Nothing
          push $ Paid $ mconcat $ map ResourcePayment resources
        ForTreachery -> do
          resources <- resourcesFor discard Nothing
          push $ Paid $ mconcat $ map ResourcePayment resources

      pure $ activeCost
        { activeCostSpentCards = discard : activeCostSpentCards activeCost
        }
    Paid payment -> do
      let
        activeCost' = activeCost
          { activeCostPayment = activeCostPayment activeCost <> payment
          }
      cards <- getAvailablePaymentSources
      abilities <- getResourceAbilities
      paid <- resourceCostPaid activeCost'
      push
        $ Ask (activeCostIdentityId activeCost)
        $ ChooseOne
        $ map PayWithCard cards
        <> map UseAbility abilities
        <> [ FinishPayment | paid ]
      pure activeCost'
    FinishedPayment -> do
      case activeCostTarget activeCost of
        ForCard card -> do
          push $ PutCardIntoPlay
            (activeCostIdentityId activeCost)
            card
            (activeCostPayment activeCost)
            (activeCostWindow activeCost)
        ForAbility _ -> pure ()
        ForTreachery -> pure ()
      pushAll
        $ map
            (DiscardedCard . PlayerCard)
            (reverse $ activeCostSpentCards activeCost)
        <> [DisableActiveCost]
      pure activeCost
    _ -> pure activeCost

resourceCostPaid :: (Projection m PlayerIdentity, HasGame m, HasQueue m, MonadRandom m, MonadThrow m) => ActiveCost -> m Bool
resourceCostPaid ActiveCost {..} = do
  let
    (rs, mrs) =
      first catMaybes $ partition isJust (costResources activeCostCost)
  prs <- paymentResources activeCostPayment
  flip evalStateT prs $ do
    l <- fmap and $ for rs $ \r -> do
      prs' <- get
      case (r `elem` prs', Wild `elem` prs') of
        (False, False) -> pure False
        (True, _) -> do
          put $ L.delete r prs'
          pure True
        (_, True) -> do
          put $ L.delete Wild prs'
          pure True
    prs' <- get
    pure $ l && length prs' >= length mrs
