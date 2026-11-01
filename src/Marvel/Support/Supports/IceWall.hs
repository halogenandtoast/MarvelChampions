module Marvel.Support.Supports.IceWall where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attack (sourceEnemy)
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage ()
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
import Marvel.Iceman.Helpers
import Marvel.Support.Types

iceWall :: SupportCard IceWall
iceWall = support IceWall Cards.iceWall

newtype IceWall = IceWall (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IceWall where
  getAbilities a = [windowAbility a 1 (W.IdentityWouldTakeAttackDamageFrom AnyEnemy AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)]
instance RunMessage IceWall where
  runMessage msg a@(IceWall attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (headMay [(you, source) | W.IdentityTakeDamageFrom you source _ <- windows]) $ \(you, source) -> do
        amount <- pendingAttackDamage you
        when (amount > 0) $ do
          cancelDamage you
          let total = supportUses attrs + amount
          if total >= 8
            then do
              removeFromPlay attrs
              for_ (sourceEnemy source) $ attachFrostbiteAfterActivation (supportController attrs)
            else addSupportUses (supportId attrs) amount
      pure a
    _ -> IceWall <$> runMessage msg attrs
