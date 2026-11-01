module Marvel.Upgrade.Upgrades.MagnetosCape where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

magnetosCape :: UpgradeCard MagnetosCape
magnetosCape = upgrade MagnetosCape Cards.magnetosCape

newtype MagnetosCape = MagnetosCape (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MagnetosCape where
  getAbilities (MagnetosCape a) = [limitedWindowAbility a 1 (W.MagneticPullSucceeded (IdentityWithId $ upgradeController a))
    Response OwnsThis ExhaustCost (runAbility a 1)]

instance HasModifiersFor MagnetosCape where
  getModifiersFor _ (IdentityRef you) _ = do
    magneto <- identityMatches (HeroIdentity <> IdentityWithTitle "Magneto") you
    pure [TraitModifier T.Aerial | magneto]
  getModifiersFor _ _ _ = pure []

instance RunMessage MagnetosCape where
  runMessage msg e@(MagnetosCape attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      magnetos <- selectList (HeroIdentity <> IdentityWithTitle "Magneto")
      traverse_ ready magnetos
      pure e
    _ -> MagnetosCape <$> runMessage msg attrs
