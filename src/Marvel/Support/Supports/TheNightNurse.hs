module Marvel.Support.Supports.TheNightNurse (
  theNightNurse,
  TheNightNurse (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

-- | Uses (3 medical counters).
theNightNurse :: SupportCard TheNightNurse
theNightNurse =
  supportWith TheNightNurse Cards.theNightNurse $ (usesL .~ 3) . (discardIfNoUsesL .~ True)

newtype TheNightNurse = TheNightNurse (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheNightNurse where
  getAbilities a =
    [ability a 1 Action OwnsThis (ExhaustCost <> UseCost) $ runAbility a 1]

-- | Heal 1 damage from a hero and discard 1 status card from it, of whichever type
-- (FAQ: a tough status card too).
instance RunMessage TheNightNurse where
  runMessage msg s@(TheNightNurse attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      heroes <- selectList HeroIdentity
      chooseOnePrompted ident "$label.chooseHeroToHeal"
        [ targeting hero do
          heal hero 1
          cardSpecific attrs "status" hero
        | hero <- heroes
        ]
      pure s
    CardSpecific (isTarget attrs -> True) "status" (fromJSON -> Success hero) -> do
      held <- filterM (\st -> member (IdentityCharacter hero) <$> select (CharacterWithStatus st)) [minBound .. maxBound]
      chooseOnePrompted (supportController attrs) "$label.chooseStatusToDiscard"
        [labeled (statusLabel st) (removeStatus (hero :: IdentityId) st) | st <- held]
      pure s
    _ -> TheNightNurse <$> runMessage msg attrs
