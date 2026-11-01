module Marvel.Obligation.Obligations.PermanentlyPhased where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Upgrade.Cards qualified as Upgrades

permanentlyPhased :: ObligationCard PermanentlyPhased
permanentlyPhased = obligation PermanentlyPhased Cards.permanentlyPhased

newtype PermanentlyPhased = PermanentlyPhased (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PermanentlyPhased where
  getAbilities (PermanentlyPhased a) =
    [ability a 1 AlterEgoAction NoCriteria ExhaustCost (runAbility a 1)]

instance HasModifiersFor PermanentlyPhased where
  getModifiersFor _ (IdentityRef you) (PermanentlyPhased a) = pure $ if Just you == obligationOwner a then [CannotAttack, CannotDefend, CannotChangeMassForm] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage PermanentlyPhased where
  runMessage msg e@(PermanentlyPhased attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      forms <- selectList (UpgradeIs Upgrades.solid <> UpgradeControlledBy (IdentityWithId you))
      for_ forms (\uid -> cardSpecific uid "flipMassForm" ())
      pure $ PermanentlyPhased attrs {obligationOwner = Just you}
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeFromGame attrs
    _ -> PermanentlyPhased <$> runMessage msg attrs
