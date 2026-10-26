module Marvel.AlterEgo.AlterEgos.NadiaVanDyne (
  nadiaVanDyne,
  NadiaVanDyne (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Stats

nadiaVanDyne :: AlterEgoCard NadiaVanDyne
nadiaVanDyne =
  alterEgo
    NadiaVanDyne
    Cards.nadiaVanDyne
    (HP $ Static 11)
    (HandSize 6)
    (Rec 3)
    [Cards.redDreams]

newtype NadiaVanDyne = NadiaVanDyne (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | G.I.R.L. — Action: Shuffle up to 2 cards with a printed [mental] resource from your
-- discard pile into your deck. (Limit once per round.)
instance HasAbilities NadiaVanDyne where
  getAbilities a =
    [ label "G.I.R.L." $
        limitedAbility a 1 (PerRound 1) Action IsSelf NoCost $ runAbility a 1
    ]

instance RunMessage NadiaVanDyne where
  runMessage msg ae@(NadiaVanDyne a) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      mental <- filter (elem Mental . printedResources . getCardDef) <$> projectMap PlayerIdentityDiscard unDiscard ident
      -- up to 2: a first card or none, then a second or none
      let prompt = "$label.chooseMentalCardToShuffle"
      chooseOnePrompted ident prompt $
        labeled "$label.done" (pure ())
          : [ targeting c $
              chooseOnePrompted ident prompt $
                labeled "$label.done" (shuffleIntoDeck ident [c])
                  : [targeting c' (shuffleIntoDeck ident [c, c']) | c' <- mental, c' /= c]
            | c <- mental
            ]
      pure ae
    _ -> NadiaVanDyne <$> runMessage msg a
