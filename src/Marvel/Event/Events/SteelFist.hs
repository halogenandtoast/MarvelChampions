module Marvel.Event.Events.SteelFist where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

steelFist :: EventCard SteelFist
steelFist = event SteelFist Cards.steelFist

newtype SteelFist = SteelFist (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SteelFist where
  runMessage msg e@(SteelFist attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs (toDamage 5 $ FromPlayerAttack you) >> cardSpecific attrs "tough" (you, enemy)) | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "tough" (fromJSON -> Success (you, enemy)) -> do
      tough <- characterMatches (CharacterWithStatus ToughStatus) (IdentityCharacter you)
      exists <- enemyMatches AnyEnemy enemy
      when (tough && exists) $ chooseOne you
        [ labeled "$label.steelFistDiscardTough" (removeStatus you ToughStatus >> stun enemy attrs >> confuse enemy attrs)
        , labeled "$label.done" (pure ())
        ]
      pure e
    _ -> SteelFist <$> runMessage msg attrs
