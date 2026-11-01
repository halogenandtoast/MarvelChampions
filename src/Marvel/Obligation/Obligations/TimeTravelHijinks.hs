module Marvel.Obligation.Obligations.TimeTravelHijinks (
  timeTravelHijinks,
  TimeTravelHijinks (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Ref
import Marvel.Resource

timeTravelHijinks :: ObligationCard TimeTravelHijinks
timeTravelHijinks = obligation TimeTravelHijinks Cards.timeTravelHijinks

newtype TimeTravelHijinks = TimeTravelHijinks (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-Ego Action: discard an [energy] resource from your hand → discard this
-- obligation, and each facedown card under it.
instance HasAbilities TimeTravelHijinks where
  getAbilities (TimeTravelHijinks a) = discardToDiscardAbility a (withResource Energy)

-- | When Revealed: discard the highest-cost card you control (of those tied, the one you
-- choose), then place it facedown under this card. It stays in your play area.
instance RunMessage TimeTravelHijinks where
  runMessage msg o@(TimeTravelHijinks attrs) = runQueueT $ case msg of
    ObligationMessage ident (RevealObligation you) | ident == obligationId attrs -> do
      attrs' <- runMessage msg attrs
      highest <- highestCostCardsControlled you
      unless (null highest) $
        chooseOrRunOne
          you
          [ targeting t do
              discardControlled t
              cardSpecific attrs "tuck" (cardIdOf t)
          | t <- highest
          ]
      pure $ TimeTravelHijinks attrs'
    CardSpecific (isTarget attrs -> True) "tuck" (fromJSON -> Success (Just cid)) -> do
      -- from its owner's discard pile, where it went
      discards <- concatMapM (fmap unDiscard . project PlayerIdentityDiscard) =<< getPlayers
      case filter ((== cid) . pcCardId) discards of
        card : _ -> do
          removeFromGame (CardIdRef cid)
          cardSpecific attrs "holdAfterRemoval" card
          pure o
        [] -> pure o
    CardSpecific (isTarget attrs -> True) "holdAfterRemoval" (fromJSON -> Success card) ->
      pure . TimeTravelHijinks $ attrs & heldCardsL <>~ [card]
    ObligationMessage ident (ResolvedObligation _) | ident == obligationId attrs -> pure o
    RanAbility you (isTarget attrs -> True) 1 _ _ -> o <$ discardToDiscard attrs you (withResource Energy)
    _ -> TimeTravelHijinks <$> runMessage msg attrs
   where
    cardIdOf = \case
      AllyRef (AllyId u) -> Just (CardId u)
      SupportRef (SupportId u) -> Just (CardId u)
      UpgradeRef (UpgradeId u) -> Just (CardId u)
      _ -> Nothing
