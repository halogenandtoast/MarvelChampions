module Marvel.Event.Events.ToeToToe (
  toeToToe,
  ToeToToe (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref

toeToToe :: EventCard ToeToToe
toeToToe = event ToeToToe Cards.toeToToe

newtype ToeToToe = ToeToToe (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Choose an enemy. That enemy attacks you. Deal 5 damage to that enemy."
instance RunMessage ToeToToe where
  runMessage msg e@(ToeToToe attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AttackableEnemy
        let
          attacks = \case
            EnemyVillainId vid -> VillainMessage vid (VillainAttacks identityId)
            EnemyMinionId mid -> MinionMessage mid (MinionAttacks identityId)
          fight enemy = case damageChoice attrs (toDamage 5 $ FromPlayerAttack identityId) enemy of
            TargetLabel target cs -> TargetLabel target (Run [attacks enemy] : cs)
            other -> other
        chooseOne identityId $ map fight enemies
        pure e
      _ -> ToeToToe <$> runMessage msg attrs
    _ -> ToeToToe <$> runMessage msg attrs
