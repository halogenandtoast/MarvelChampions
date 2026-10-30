module Marvel.Obligation.Obligations.CorruptedProgramming where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Keyword (Keyword (MassForm))
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W

corruptedProgramming :: ObligationCard CorruptedProgramming
corruptedProgramming = obligation CorruptedProgramming Cards.corruptedProgramming

newtype CorruptedProgramming = CorruptedProgramming (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor CorruptedProgramming where
  getBlankingModifiersFor _ (UpgradeRef uid) (CorruptedProgramming a) = case obligationOwner a of
    Nothing -> pure []
    Just owner -> do
      matches <- upgradeMatches (UpgradeWithId uid <> UpgradeWithKeyword MassForm <> UpgradeControlledBy (IdentityWithId owner)) uid
      pure [Blank | matches]
  getBlankingModifiersFor _ _ _ = pure []

instance HasAbilities CorruptedProgramming where
  getAbilities (CorruptedProgramming a) = case obligationOwner a of
    Nothing -> []
    Just _ -> [ability a 1 AlterEgoAction NoCriteria ExhaustCost (runAbility a 1)]

instance RunMessage CorruptedProgramming where
  runMessage msg o@(CorruptedProgramming attrs) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> o <$ removeFromGame attrs
    _ -> CorruptedProgramming <$> runMessage msg attrs
