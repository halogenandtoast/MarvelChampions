module Marvel.Obligation.Obligations.TroubleInOtherworld (troubleInOtherworld, TroubleInOtherworld (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Upgrades

troubleInOtherworld :: ObligationCard TroubleInOtherworld
troubleInOtherworld = obligation TroubleInOtherworld Cards.troubleInOtherworld

newtype TroubleInOtherworld = TroubleInOtherworld (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor TroubleInOtherworld where
  getModifiersFor _ (IdentityRef ident) (TroubleInOtherworld attrs) | obligationOwner attrs == Just ident = do
    marked <- selectList (EnemyWithUpgrade $ UpgradeIs Upgrades.deathGlow <> upgradeControlledBy ident)
    pure [CannotAttackEnemy enemy | enemy <- marked]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TroubleInOtherworld where
  getAbilities a@(TroubleInOtherworld attrs) =
    [ ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId owner)
        (MultiResourceCost [Just Energy, Just Mental]) (runAbility a 1)
    | Just owner <- [obligationOwner attrs]
    ]

instance RunMessage TroubleInOtherworld where
  runMessage msg o@(TroubleInOtherworld attrs) = runQueueT $ case msg of
    -- It remains in Brunnhilde's play area, rather than being discarded after reveal.
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> o <$ removeFromGame attrs
    _ -> TroubleInOtherworld <$> runMessage msg attrs
