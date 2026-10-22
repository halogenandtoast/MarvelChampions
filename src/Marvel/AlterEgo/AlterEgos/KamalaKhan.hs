module Marvel.AlterEgo.AlterEgos.KamalaKhan where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Allies
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Event.Cards qualified as Events
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Stats
import Marvel.Support.Cards qualified as Supports
import Marvel.Upgrade.Cards qualified as Upgrades

kamalaKhan :: AlterEgoCard KamalaKhan
kamalaKhan =
  alterEgo
    KamalaKhan
    Cards.kamalaKhan
    (HP $ Static 10)
    (HandSize 6)
    (Rec 5)
    [Cards.homeByDawn]

newtype KamalaKhan = KamalaKhan (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | A Ms. Marvel card: one of her identity-specific cards.
msMarvelCard :: CardMatcher
msMarvelCard =
  CardMatchesAny $
    map
      CardIs
      [ Allies.redDagger
      , Events.bigHands
      , Events.sneakBy
      , Events.wiggleRoom
      , Supports.aamirKhan
      , Supports.brunoCarrelli
      , Supports.nakiaBahadir
      , Upgrades.biokineticPolymerSuit
      , Upgrades.embiggen
      , Upgrades.shrink
      ]

instance HasAbilities KamalaKhan where
  getAbilities a =
    [ label "Teen Spirit" $
        limitedAbility a 1 (PerRound 1) Action IsSelf NoCost (runAbility a 1)
    ]

instance RunMessage KamalaKhan where
  runMessage msg a@(KamalaKhan attrs) = case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      push $ IdentityMessage ident $ DiscardFromDeckUntil msMarvelCard (toTarget a)
      pure a
    WithDiscardedMatch (isTarget a -> True) _ (PlayerCard card) -> do
      push $ IdentityMessage (alterEgoIdentityId attrs) (AddToHand card)
      pure a
    _ -> KamalaKhan <$> runMessage msg attrs
