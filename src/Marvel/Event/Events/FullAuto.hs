module Marvel.Event.Events.FullAuto (fullAuto, FullAuto (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Payment

fullAuto :: EventCard FullAuto
fullAuto = event FullAuto Cards.fullAuto

newtype FullAuto = FullAuto (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

autoTargets :: (HasGame m) => Attrs Event -> m [EnemyId]
autoTargets attrs = filterM (fmap not . (\enemy -> cannotTakeDamageFrom (enemyRef enemy) (toSource attrs)))
  =<< selectList (AttackableBy $ IdentityWithId $ eventController attrs)

instance IsEvent FullAuto where
  -- Choosing the enemy is before the arrow: retain it in TargetPayment while the
  -- resources and ammo are paid. A stun replaces the effect, not this chosen cost.
  eventTargets (FullAuto attrs) = do
    stunned <- selectAny (IdentityWithId (eventController attrs) <> StunnedIdentity)
    enemies <- if stunned then selectList AnyEnemy else autoTargets attrs
    pure $ Just $ map enemyRef enemies

instance RunMessage FullAuto where
  runMessage msg e@(FullAuto attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      let chosen = case paymentTarget payment of
            Just (VillainRef vid) -> Just (EnemyVillainId vid)
            Just (MinionRef mid) -> Just (EnemyMinionId mid)
            Just (EnemyRef enemy) -> Just enemy
            _ -> Nothing
      eligible <- autoTargets attrs
      for_ [enemy | Just enemy <- [chosen], enemy `elem` eligible] \enemy ->
        dealDamage enemy attrs (withOverkill $ toDamage 8 $ FromPlayerAttack you)
      pure e
    _ -> FullAuto <$> runMessage msg attrs
