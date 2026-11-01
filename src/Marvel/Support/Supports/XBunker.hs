module Marvel.Support.Supports.XBunker where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import {-# SOURCE #-} Marvel.Game (sideSchemesInVictoryDisplay)

xBunker :: SupportCard XBunker
xBunker = support XBunker Cards.xBunker

newtype XBunker = XBunker (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities XBunker where
  getAbilities a = [ability a 1 Action (OwnsThis <> CharacterExists (IdentityCharacters $ IdentityWithTrait Trait.Mutant)) ExhaustCost (runAbility a 1)]

instance RunMessage XBunker where
  runMessage msg e@(XBunker a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      players <- selectList (IdentityWithTrait Trait.Mutant)
      n <- sideSchemesInVictoryDisplay
      chooseOne you [targeting p $ search p (SearchIdentityDeck p $ TopOfDeck $ fromIntegral n) AnyCard SearchDrawOne ShuffleBackIn | p <- players]
      pure e
    _ -> XBunker <$> runMessage msg a
