module Marvel.Obligation.Obligations.HotHeaded where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Obligation.Types

hotHeaded :: ObligationCard HotHeaded
hotHeaded = obligation HotHeaded Cards.hotHeaded

newtype HotHeaded = HotHeaded (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities HotHeaded where
  getAbilities a@(HotHeaded attrs) = case obligationOwner attrs of
    Nothing -> []
    Just you -> [limitedWindowAbility a 2
      (W.AttachesUpgradeToEnemy W.After (IdentityWithId you) (CardIs Cards.frostbite) AnyEnemy)
      ForcedResponse OwnsThis NoCost (runAbility a 2)
      , limitedWindowAbility a 1 (W.UsesRecoveryAt W.After $ IdentityWithId you) AlterEgoResponse OwnsThis NoCost (runAbility a 1)]
instance RunMessage HotHeaded where
  runMessage msg a@(HotHeaded attrs) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure a
    RanAbility you (isTarget attrs -> True) 2 _ _ -> a <$ damageIdentity you attrs (toDamage 1 FromAbility)
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromPlay attrs
    _ -> HotHeaded <$> runMessage msg attrs
