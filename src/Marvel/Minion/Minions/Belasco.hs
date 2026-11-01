module Marvel.Minion.Minions.Belasco where

import Marvel.Prelude
import Marvel.Query
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Card
import Marvel.Domino (cardInDeckOrDiscard)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Window qualified as W

belasco :: MinionCard Belasco
belasco = minion Belasco Cards.belasco (Sch 1) (Atk 1) (HP 6)

newtype Belasco = Belasco (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Belasco where
  getAbilities a@(Belasco attrs) = [limitedWindowAbility a 1
    (W.EnemyActivates W.After (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity)
    ForcedResponse NoCriteria NoCost (runAbility a 1)]

instance RunMessage Belasco where
  runMessage msg a@(Belasco attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [you | W.EnemyActivation _ you <- windows] (\you -> discardFromDeck you 3 attrs)
      pure a
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      ruler <- selectOne (SideSchemeIs Schemes.rulerOfLimbo)
      for_ ruler \scheme -> for_ [card | PlayerCard card <- cards] \card -> do
        stillAvailable <- maybe (pure False) (\owner -> cardInDeckOrDiscard owner card) (pcOwner card)
        when stillAvailable $ holdFacedownCard scheme card
      pure a
    _ -> Belasco <$> runMessage msg attrs
