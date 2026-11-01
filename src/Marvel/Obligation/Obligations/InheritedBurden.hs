module Marvel.Obligation.Obligations.InheritedBurden where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Entity
import Marvel.Matchers
    ( IdentityMatcher(UnexhaustedIdentity, HeroIdentity,
                      AlterEgoIdentity),
      UpgradeMatcher(UpgradeThatCanBeDiscarded, UpgradeWithTrait),
      upgradeControlledBy,
      identityMatches )
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query
import Marvel.Trait


inheritedBurden :: ObligationCard InheritedBurden
inheritedBurden = obligation InheritedBurden Cards.inheritedBurden

newtype InheritedBurden = InheritedBurden (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage InheritedBurden where
  runMessage msg e@(InheritedBurden attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hero <- identityMatches HeroIdentity you
      when hero $ chooseOne you
        [ labeled "$label.doNotFlipToAlterEgo" (pure ())
        , labeled "$label.changeToB" (changeToAlterEgo you)
        ]
      resolveObligation (obligationId attrs) you
      pure e
    ObligationMessage oid (ResolveObligation you) | oid == obligationId attrs -> do
      ready <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      interfaces <- selectList $ upgradeControlledBy you <> UpgradeWithTrait Interface <> UpgradeThatCanBeDiscarded
      let choices =
            [labeled "$label.exhaustPeniParkerRemoveInheritedBurden" (exhaust you >> removeFromGame attrs) | ready]
            <> [labeled "$label.discardInterfaceUpgrade"
                  (chooseOne you [targeting uid (discardUpgrade uid) | uid <- interfaces]) | notNull interfaces]
      unless (null choices) $ chooseOrRunOne you choices
      pure e
    _ -> InheritedBurden <$> runMessage msg attrs
