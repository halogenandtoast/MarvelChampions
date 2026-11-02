{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50017 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

aosP50017 :: SupportCard AosP50017
aosP50017 = supportWith AosP50017 Cards.aosP50017 ((usesL .~ 2) . (discardIfNoUsesL .~ True))

newtype AosP50017 = AosP50017 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50017 where
  getAbilities (AosP50017 attrs) =
    [ability attrs 1 Action (OwnsThis) (ExhaustCost <> UseCost) (runAbility attrs 1)]

instance RunMessage AosP50017 where
  runMessage msg card@(AosP50017 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      chooseOne you [targeting p do
        cards <- selectList (InHandOf (IdentityWithId p) $ BasicCardMatches $ CardWithType AllyType)
        chooseOne p [targeting c (putIntoPlay p c) | c <- cards]
        | p <- players]
      pure card
    _ -> AosP50017 <$> runMessage msg attrs
