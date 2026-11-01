module Marvel.Event.Events.ThisCardIsFire where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Id (CardId (..))
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id (EventId (..), CharacterId (IdentityCharacter))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.Window qualified as W

thisCardIsFire :: EventCard ThisCardIsFire
thisCardIsFire = event ThisCardIsFire Cards.thisCardIsFire

newtype ThisCardIsFire = ThisCardIsFire (Attrs Event)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

fireCardId :: Attrs Event -> CardId
fireCardId = CardId . unEventId . eventId

instance IsEvent ThisCardIsFire where
  eventHandAbilities (ThisCardIsFire attrs) =
    [ windowAbility (CardIdRef (fireCardId attrs) :: Source) 1
        (W.TurnEnds W.After $ IdentityWithId $ eventController attrs)
        ForcedResponse NoCost (RunAbility (CardIdRef $ fireCardId attrs) 1)
    ]
  eventTargets (ThisCardIsFire attrs) = do
    amount <- characterStoredDamage (IdentityCharacter $ eventController attrs)
    stunned <- selectAny (IdentityWithId (eventController attrs) <> StunnedIdentity)
    pure $ if amount > 0 || stunned then Nothing else Just []

instance RunMessage ThisCardIsFire where
  runMessage msg e@(ThisCardIsFire attrs) = runQueueT $ case msg of
    RanAbility you (CardIdRef cid) 1 _ _ | cid == fireCardId attrs -> do
      damageIdentity you (CardIdRef cid) (toDamage 1 FromAbility)
      pure e
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      amount <- characterStoredDamage (IdentityCharacter you)
      chooseDamage you attrs (toDamage amount $ FromPlayerAttack you) (AttackableBy $ IdentityWithId you)
      pure e
    _ -> ThisCardIsFire <$> runMessage msg attrs
