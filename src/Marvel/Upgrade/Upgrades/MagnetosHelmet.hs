module Marvel.Upgrade.Upgrades.MagnetosHelmet where

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
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types qualified as R
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

magnetosHelmet :: UpgradeCard MagnetosHelmet
magnetosHelmet = upgrade MagnetosHelmet Cards.magnetosHelmet

newtype MagnetosHelmet = MagnetosHelmet (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MagnetosHelmet where
  getAbilities a = [ability a 1 Resource (OwnsThis <> PayingFor (CardWithTrait T.Magnetic)) ExhaustCost (Pay $ ResourcePayment R.Wild)]

instance HasModifiersFor MagnetosHelmet where
  getModifiersFor _ (IdentityRef you) _ = do
    magneto <- identityMatches (HeroIdentity <> IdentityWithTitle "Magneto") you
    pure [KeywordModifier Steady | magneto]
  getModifiersFor _ _ _ = pure []

instance RunMessage MagnetosHelmet where
  runMessage msg (MagnetosHelmet attrs) = runQueueT $ case msg of
    _ -> MagnetosHelmet <$> runMessage msg attrs
