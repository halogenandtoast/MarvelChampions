module Marvel.AlterEgo.AlterEgos.BruceBanner where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Stats

bruceBanner :: AlterEgoCard BruceBanner
bruceBanner =
  alterEgo
    BruceBanner
    Cards.bruceBanner
    (HP $ Static 18)
    (HandSize 5)
    (Rec 4)
    [Cards.innerDemons]

newtype BruceBanner = BruceBanner (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities BruceBanner where
  getAbilities a =
    [ label "Experimental Research" $
        limitedAbility a 1 (PerRound 1) Action IsSelf NoCost $
          runAbility a 1
    ]

instance RunMessage BruceBanner where
  runMessage msg ae@(BruceBanner a) = case msg of
    -- draw 1 card, then choose and discard 1 card from your hand
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      pushAll
        [ IdentityMessage ident (DrawCards FromDeck 1)
        , IdentityMessage ident (DiscardFrom FromHand 1 Nothing)
        ]
      pure ae
    _ -> BruceBanner <$> runMessage msg a
