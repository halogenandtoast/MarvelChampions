module Marvel.AlterEgo.AlterEgos.MonicaRambeau (
  monicaRambeau,
  MonicaRambeau (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

monicaRambeau :: AlterEgoCard MonicaRambeau
monicaRambeau =
  alterEgo
    MonicaRambeau
    Cards.monicaRambeau
    (HP $ Static 11)
    (HandSize 6)
    (Rec 3)
    [Cards.lossOfControl]

newtype MonicaRambeau = MonicaRambeau (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Setup: Put all 3 energy form upgrades into play, facedown. Power Down — Forced
-- Response: After you change to this form, turn all your energy form upgrades facedown.
instance HasAbilities MonicaRambeau where
  getAbilities a@(MonicaRambeau attrs) =
    [ ability a 1 Setup NoCriteria NoCost $ runAbility a 1
    , label "Power Down" $
        limitedWindowAbility
          a
          2
          (W.IdentityChangedToForm W.After $ IdentityWithId $ alterEgoIdentityId attrs)
          ForcedResponse
          IsSelf
          NoCost
          (runAbility a 2)
    ]

instance RunMessage MonicaRambeau where
  runMessage msg ae@(MonicaRambeau attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      putEnergyFormsIntoPlay ident
      pure ae
    RanAbility ident (isTarget attrs -> True) 2 _ _ -> do
      turnEnergyFormsFacedown ident
      pure ae
    _ -> MonicaRambeau <$> runMessage msg attrs
