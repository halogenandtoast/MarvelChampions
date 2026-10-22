module Marvel.Support.Supports.Quinjet (
  quinjet,
  Quinjet (..),
) where

import Marvel.Prelude

import Data.Maybe (maybeToList)

import Marvel.Ability
import Marvel.Card
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Identity (uniqueMatch)
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Name
import Marvel.Payment.Types (Payment (NoPayment))
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait
import Marvel.Window

quinjet :: SupportCard Quinjet
quinjet = support Quinjet Cards.quinjet

newtype Quinjet = Quinjet (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The Avenger allies in the player's hand it could put into play: their printed cost
-- no more than its time counters.
avengersInHand :: Attrs Support -> ExtendedCardMatcher
avengersInHand a =
  InHandOf (IdentityWithId $ supportController a) $
    BasicCardMatches $
      CardWithType AllyType <> CardWithTrait Avenger <> CardWithPrintedCostAtMost (supportUses a)

instance HasAbilities Quinjet where
  getAbilities (Quinjet a) =
    [ limitedWindowAbility a 1 (TurnBegins $ IdentityWithId $ supportController a) Response OwnsThis NoCost $
        runAbility a 1
    , ability a 2 Action (OwnsThis <> ExtendedCardExists (avengersInHand a)) NoCost $ runAbility a 2
    ]

-- | Its time counters are kept as its uses.
instance RunMessage Quinjet where
  runMessage msg s@(Quinjet attrs) = case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> pure $ Quinjet $ attrs & usesL +~ 1
    RanAbility ident (isTarget attrs -> True) 2 _ _ -> do
      cards <- selectList (avengersInHand attrs)
      -- a unique ally whose title is in play cannot enter it
      inPlay <- getUniqueTitlesInPlay
      let
        named c = (title $ cdName $ getCardDef c, maybeToList $ subtitle $ cdName $ getCardDef c)
        enters c = not (cdUnique $ getCardDef c) || not (any (uniqueMatch (named c)) inPlay)
      chooseOne ident
        [ TargetLabel
          (toRef c)
          -- discarded before what answers the ally entering play
          [ Run
              [ RemoveFromPlay (toTarget attrs)
              , DiscardedCard (toCard attrs)
              , PutCardIntoPlay ident c NoPayment Nothing
              ]
          ]
        | c <- filter enters cards
        ]
      pure s
    _ -> Quinjet <$> runMessage msg attrs
