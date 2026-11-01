module Marvel.Ally.Allies.NegasonicTeenageWarhead where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Query ()
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Stats ()

negasonicTeenageWarhead :: AllyCard NegasonicTeenageWarhead
negasonicTeenageWarhead = ally NegasonicTeenageWarhead Cards.negasonicTeenageWarhead (Thw 2, 0) (Atk 2, 0) (HP 4)

newtype NegasonicTeenageWarhead = NegasonicTeenageWarhead (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities NegasonicTeenageWarhead where
  getAbilities a =
    [limitedWindowAbility a 1
      (W.WindowOneOf [W.TreacheryRevealed W.When AnyTreachery source
        | source <- [W.RevealedFromEncounterDeck, W.RevealedFromVillain, W.RevealedFromDiscardPile, W.RevealedFromSetAside, W.RevealedFromRemovedFromGame]])
      Interrupt (OwnsThis <> CanCancelRevealedTreachery) (DealDamageThisCost 2) (runAbility a 1)]

instance RunMessage NegasonicTeenageWarhead where
  runMessage msg e@(NegasonicTeenageWarhead attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [tid | W.RevealTreachery tid _ <- windows] cancelWhenRevealed
      pure e
    _ -> NegasonicTeenageWarhead <$> runMessage msg attrs
