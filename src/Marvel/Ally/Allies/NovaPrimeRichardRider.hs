module Marvel.Ally.Allies.NovaPrimeRichardRider (
  novaPrimeRichardRider,
  NovaPrimeRichardRider (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (Elite))
import Marvel.Window

novaPrimeRichardRider :: AllyCard NovaPrimeRichardRider
novaPrimeRichardRider = ally NovaPrimeRichardRider Cards.novaPrimeRichardRider (Thw 2, 1) (Atk 3, 1) (HP 3)

newtype NovaPrimeRichardRider = NovaPrimeRichardRider (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

nonElite :: MinionMatcher
nonElite = NotMinion (MinionWithTrait Elite)

-- | "Response: After you play Nova Prime from your hand, defeat a non-Elite minion."
-- ponytail: answers the after-played-ally window as Elektra does; that window does not
-- tell a card played from hand from one put into play.
instance HasAbilities NovaPrimeRichardRider where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response (OwnsThis <> MinionExists nonElite) NoCost $
        runAbility a 1
    ]

instance RunMessage NovaPrimeRichardRider where
  runMessage msg a@(NovaPrimeRichardRider attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      minions <- selectList nonElite
      chooseOne ident [targeting m (defeatMinionBy attrs m) | m <- minions]
      pure a
    _ -> NovaPrimeRichardRider <$> runMessage msg attrs
