module Marvel.Obligation.Obligations.Weakened (
  weakened,
  Weakened (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window

weakened :: ObligationCard Weakened
weakened = obligation Weakened Cards.weakened

newtype Weakened = Weakened (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Response: after you use a basic hero power (thwart, attack or defense), take
-- 1 damage. Alter-Ego Action: discard a [physical] resource from your hand → discard
-- this obligation.
instance HasAbilities Weakened where
  getAbilities (Weakened a) = case obligationOwner a of
    Just you ->
      windowAbility
        a
        2
        ( WindowOneOf
            [ MakesBasicAttack After (IdentityWithId you)
            , MakesBasicThwart After (IdentityWithId you)
            , HeroDefended After (IdentityWithId you) AnyEnemy
            ]
        )
        ForcedResponse
        NoCost
        (runAbility a 2)
        : discardToDiscardAbility a (withResource Physical)
    Nothing -> []

-- | It stays in your play area once revealed.
instance RunMessage Weakened where
  runMessage msg o@(Weakened attrs) = runQueueT $ case msg of
    ObligationMessage ident (ResolvedObligation _) | ident == obligationId attrs -> pure o
    RanAbility you (isTarget attrs -> True) 1 _ _ -> o <$ discardToDiscard attrs you (withResource Physical)
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      for_ (obligationOwner attrs) \you -> damageIdentity you attrs (toDamage 1 FromAbility)
      pure o
    _ -> Weakened <$> runMessage msg attrs
