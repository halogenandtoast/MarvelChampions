module Marvel.AlterEgo.AlterEgos.DraxAlterEgo (
  draxAlterEgo,
  DraxAlterEgo (..),
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
import Marvel.Id ()
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

draxAlterEgo :: AlterEgoCard DraxAlterEgo
draxAlterEgo =
  alterEgo
    DraxAlterEgo
    Cards.draxAlterEgo
    (HP $ Static 14)
    (HandSize 6)
    (Rec 4)
    [Cards.memoriesOfAnotherLife]

newtype DraxAlterEgo = DraxAlterEgo (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Forced Response: after you change to this form, remove all vengeance counters from
-- Drax, and heal 2 damage from him for each removed.
instance HasAbilities DraxAlterEgo where
  getAbilities a@(DraxAlterEgo attrs) =
    [ limitedWindowAbility
        a
        1
        (W.IdentityChangedToForm W.After $ IdentityWithId $ alterEgoIdentityId attrs)
        ForcedResponse
        IsSelf
        NoCost
        (runAbility a 1)
    ]

instance RunMessage DraxAlterEgo where
  runMessage msg ae@(DraxAlterEgo attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      n <- project PlayerIdentityCounters ident
      when (n > 0) do
        removeCounters (IdentityRef ident) n
        heal (IdentityCharacter ident) (2 * n)
      pure ae
    _ -> DraxAlterEgo <$> runMessage msg attrs
