module Marvel.Obligation.Obligations.GuildBusiness where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

guildBusiness :: ObligationCard GuildBusiness
guildBusiness = obligation GuildBusiness Cards.guildBusiness

newtype GuildBusiness = GuildBusiness (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GuildBusiness where
  getAbilities (GuildBusiness attrs) =
    [ability attrs 1 AlterEgoAction (SelfMatches $ IdentityWithId owner)
      (ExhaustCost <> ResourceCost (Just Energy)) (runAbility attrs 1)
    | Just owner <- [obligationOwner attrs]]

instance RunMessage GuildBusiness where
  runMessage msg o@(GuildBusiness attrs) = runQueueT $ case msg of
    -- This obligation stays in its owner's play area until its printed action removes it.
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> o <$ removeFromGame attrs
    _ -> GuildBusiness <$> runMessage msg attrs
