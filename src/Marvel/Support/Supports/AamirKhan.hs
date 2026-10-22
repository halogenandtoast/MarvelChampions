module Marvel.Support.Supports.AamirKhan (
  aamirKhan,
  AamirKhan (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Identity.Types
import Marvel.Message
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

aamirKhan :: SupportCard AamirKhan
aamirKhan = support AamirKhan Cards.aamirKhan

newtype AamirKhan = AamirKhan (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AamirKhan where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

-- | A card from the discard pile goes to the bottom of the deck, then one is drawn: with
-- nothing in the discard pile, the card is still drawn.
instance RunMessage AamirKhan where
  runMessage msg a@(AamirKhan attrs) = case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      discards <- projectMap PlayerIdentityDiscard unDiscard ident
      let draw = IdentityMessage ident $ DrawCards FromDeck 1
      if null discards
        then push draw
        else
          pushAll
            [ FocusCards $ map PlayerCard discards
            , Ask ident $
                ChooseOne
                  [ TargetLabel (toRef c) [Run [IdentityMessage ident (PlaceOnBottomOfDeck c)]]
                  | c <- discards
                  ]
            , UnfocusCards
            , draw
            ]
      pure a
    _ -> AamirKhan <$> runMessage msg attrs
