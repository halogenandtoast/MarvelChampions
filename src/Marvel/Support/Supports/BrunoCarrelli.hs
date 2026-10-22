module Marvel.Support.Supports.BrunoCarrelli (
  brunoCarrelli,
  BrunoCarrelli (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Message
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

brunoCarrelli :: SupportCard BrunoCarrelli
brunoCarrelli = support BrunoCarrelli Cards.brunoCarrelli

newtype BrunoCarrelli = BrunoCarrelli (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BrunoCarrelli where
  getAbilities a =
    [ ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)
    , ability a 2 Action OwnsThis ExhaustCost (runAbility a 2)
    ]

-- | The cards are held here facedown, out of the hand, until taken back or discarded
-- with Bruno.
instance RunMessage BrunoCarrelli where
  runMessage msg a@(BrunoCarrelli attrs) = case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      hand <- projectMap PlayerIdentityHand unHand ident
      chooseOne ident [TargetLabel (toRef c) [Run [WithChosen (toTarget a) FromHand [PlayerCard c]]] | c <- hand]
      pure a
    WithChosen (isTarget a -> True) FromHand cards -> do
      let held = [c | PlayerCard c <- cards]
      pushAll [RemoveFromGame (CardIdRef $ pcCardId c) | c <- held]
      pure . BrunoCarrelli $ attrs & heldCardsL <>~ held
    RanAbility ident (isTarget a -> True) 2 _ _ -> do
      push $ cardSpecific a "take" (ident, 3 :: Natural)
      pure a
    -- up to 3, one at a time
    CardSpecific (isTarget a -> True) "take" (fromJSON -> Success (ident, n :: Natural)) -> do
      let held = supportHeldCards attrs
      when (n > 0 && notNull held) $
        chooseOne ident $
          Label "$label.done" []
            : [ TargetLabel (toRef c) [Run [IdentityMessage ident (AddToHand c), cardSpecific a "take" (ident, n - 1)]]
              | c <- held
              ]
      pure a
    IdentityMessage _ (AddToHand c) ->
      pure . BrunoCarrelli $ attrs & heldCardsL %~ filter (/= c)
    _ -> BrunoCarrelli <$> runMessage msg attrs
