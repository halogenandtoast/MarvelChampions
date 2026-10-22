module Marvel.Event.Events.DropKick (
  dropKick,
  DropKick (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

dropKick :: EventCard DropKick
dropKick = event DropKick Cards.dropKick

newtype DropKick = DropKick (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 4 damage to an enemy; paid for using only [physical] resources, stun that
-- enemy and draw 1 card.
instance RunMessage DropKick where
  runMessage msg e@(DropKick attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payment _ -> do
        physical <- paidUsingOnly Physical payment
        enemies <- selectList AttackableEnemy
        let
          kick enemy = case damageChoice attrs (toDamage 4 $ FromPlayerAttack identityId) enemy of
            TargetLabel target cs ->
              TargetLabel target $
                cs
                  <> [Stun (toRef enemy) (toSource attrs) | physical]
                  <> [Run [IdentityMessage identityId (DrawCards FromDeck 1)] | physical]
            other -> other
        chooseOne identityId $ map kick enemies
        pure e
      _ -> DropKick <$> runMessage msg attrs
    _ -> DropKick <$> runMessage msg attrs
