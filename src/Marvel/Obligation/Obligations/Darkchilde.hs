module Marvel.Obligation.Obligations.Darkchilde where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Damage
import Marvel.Matchers
import Marvel.Query

darkchilde :: ObligationCard Darkchilde
darkchilde = obligation Darkchilde Cards.darkchilde

newtype Darkchilde = Darkchilde (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Darkchilde where
  runMessage msg a@(Darkchilde attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hero <- identityMatches HeroIdentity you
      when hero $ chooseOne you
        [ labeled "$label.doNotFlipToAlterEgo" (pure ())
        , labeled "$label.changeToB" (changeToAlterEgo you)
        ]
      resolveObligation oid you
      pure a
    ObligationMessage oid (ResolveObligation you) | oid == obligationId attrs -> do
      removable <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      chooseOne you $
        [labeled "$label.magikExhaustIllyanaRemoveDarkchilde" (exhaust you >> removeFromGame attrs) | removable]
        <> [labeled "$label.magikDarkchildeDamageCharacters" do
          allies <- selectList (AllyControlledBy $ IdentityWithId you)
          damageIdentity you attrs (toDamage 1 FromAbility)
          for_ allies (\ally -> damageCharacter ally attrs (toDamage 1 FromAbility))
          removeFromPlay attrs]
      pure a
    _ -> Darkchilde <$> runMessage msg attrs
