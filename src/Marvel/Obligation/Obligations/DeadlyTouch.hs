module Marvel.Obligation.Obligations.DeadlyTouch where

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
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Damage
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

deadlyTouch :: ObligationCard DeadlyTouch
deadlyTouch = obligation DeadlyTouch Cards.deadlyTouch

newtype DeadlyTouch = DeadlyTouch (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage DeadlyTouch where
  runMessage msg o@(DeadlyTouch attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation revealedFor) | oid == obligationId attrs -> do
      owner <- selectOne $ IdentityIs Heroes.rogueHero
      let you = fromMaybe revealedFor owner
      host <- touchedHost you
      case host of
        Just character@(AllyCharacter _) -> damageCharacter character attrs (toDamage 2 FromAbility)
        Just character@(IdentityCharacter _) -> damageCharacter character attrs (toDamage 2 FromAbility)
        _ -> resolveAs you $ PlaceThreat (toSource attrs) 2 EncounterMainScheme
      pure o
    _ -> DeadlyTouch <$> runMessage msg attrs
