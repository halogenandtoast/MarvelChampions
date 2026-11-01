module Marvel.Event.Events.CreativeSolution (
  creativeSolution,
  CreativeSolution (..),
) where

import Marvel.Prelude

import Data.List (nub)
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status ()

creativeSolution :: EventCard CreativeSolution
creativeSolution = event CreativeSolution Cards.creativeSolution

newtype CreativeSolution = CreativeSolution (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: draw 1 card and remove a status card from any character, friend or
-- foe. By the type removed: tough, 3 damage to an enemy; stun, 3 threat removed from a
-- scheme; confuse, 3 damage healed from an identity. With no status card in play, only
-- the draw.
instance RunMessage CreativeSolution where
  runMessage msg e@(CreativeSolution attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      drawCards you 1
      characters <- nub <$> concatMapM (selectList . CharacterWithStatus) statuses
      chooseOne you [targeting (characterRef c) (cardSpecific attrs "character" c) | c <- characters]
      pure e
    CardSpecific (isTarget attrs -> True) "character" (fromJSON -> Success (c :: CharacterId)) -> do
      held <- filterM (\s -> member c <$> select (CharacterWithStatus s)) statuses
      chooseOnePrompted (eventController attrs) "$label.chooseStatusToDiscard"
        [ labeled (statusLabel s) do
            removeStatus c s
            cardSpecific attrs "removed" s
        | s <- held
        ]
      pure e
    CardSpecific (isTarget attrs -> True) "removed" (fromJSON -> Success s) -> do
      let you = eventController attrs
      case s of
        ToughStatus -> chooseDamage you attrs (toDamage 3 FromAbility) AnyEnemy
        StunnedStatus -> removeThreat you attrs 3 ThwartableScheme
        ConfusedStatus -> resolveAs you $ ChooseHeal 3 (IdentityCharacters AnyIdentity)
      pure e
    _ -> CreativeSolution <$> runMessage msg attrs
   where
    statuses = [minBound .. maxBound]
