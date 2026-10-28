-- | Ebony Maw's Spell environments (The Mad Titan's Shadow, scenario 1): each in the
-- play area of the player who revealed it or put it into play, with its invocation
-- counters (MTS rulebook p.6).
module Marvel.Environment.Environments.EbonyMawSpells (
  fireball,
  manipulation,
  pacification,
  rubblestorm,
  EbonyMawSpell (..),
  spellEnvironment,
  eachPlayerPutsSpellIntoPlay,
  removeInvocationCounters,
) where

import Marvel.Prelude

import Marvel.Ability.Types
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Id (IdentityId)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

fireball :: EnvironmentCard EbonyMawSpell
fireball = environment EbonyMawSpell Cards.fireball

manipulation :: EnvironmentCard EbonyMawSpell
manipulation = environment EbonyMawSpell Cards.manipulation

pacification :: EnvironmentCard EbonyMawSpell
pacification = environment EbonyMawSpell Cards.pacification

rubblestorm :: EnvironmentCard EbonyMawSpell
rubblestorm = environment EbonyMawSpell Cards.rubblestorm

newtype EbonyMawSpell = EbonyMawSpell (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | "A Spell card" of the encounter deck.
spellEnvironment :: CardMatcher
spellEnvironment = CardWithType EnvironmentType <> CardWithTrait Spell

-- | "Each player discards cards from the top of the encounter deck until they discard a
-- Spell card and puts that card into play in their play area", from the first player.
eachPlayerPutsSpellIntoPlay :: (ReverseQueue m) => m ()
eachPlayerPutsSpellIntoPlay = traverse_ (`discardUntilIntoPlay` spellEnvironment) =<< getPlayers

-- | "Remove an invocation counter from each Spell in your play area": the Spells losing
-- their last one respond in the order the first player chooses (RR Simultaneous
-- Resolution), each before the next loses its counter.
removeInvocationCounters :: (ReverseQueue m) => IdentityId -> m ()
removeInvocationCounters you = do
  spells <- selectList (EnvironmentInPlayAreaOf you)
  lasts <- filterM (fmap (<= 1) . getCountersOn) spells
  for_ (filter (`notElem` lasts) spells) (`removeCounters` 1)
  case lasts of
    [s] -> removeCounters s 1
    _ -> do
      first <- selectJust FirstPlayer
      chooseOneAtATimePrompted first "$label.chooseSpellOrder" [targeting s (removeCounters s 1) | s <- lasts]

-- | Enters play with its invocation counters. Forced Response: after the last is
-- removed, discard it → its effect on "you", the player whose play area it was in.
instance RunMessage EbonyMawSpell where
  runMessage msg e@(EbonyMawSpell attrs) = runQueueT $ case msg of
    EnvironmentMessage eid EnvironmentEntered | eid == environmentId attrs -> do
      placeCounters attrs invocations
      pure e
    RemoveCounters target n | target == toRef attrs -> do
      when (n > 0 && environmentCounters attrs > 0 && subtractNatural n (environmentCounters attrs) == 0) do
        removeFromPlay attrs
        for_ (environmentOwner attrs) invoke
      EbonyMawSpell <$> runMessage msg attrs
    _ -> EbonyMawSpell <$> runMessage msg attrs
   where
    code = toCardCode attrs
    invocations
      | code == toCardCode Cards.fireball = 4
      | code == toCardCode Cards.manipulation = 2
      | otherwise = 3
    invoke you
      -- deal 4 damage to your identity
      | code == toCardCode Cards.fireball = damageIdentity you attrs (toDamage 4 FromAbility)
      -- discard 1 card at random from your hand; you are confused
      | code == toCardCode Cards.manipulation = do
          discardFrom you RandomFromHand 1
          confuseIdentity you
      -- exhaust each upgrade you control; you are stunned
      | code == toCardCode Cards.pacification = do
          traverse_ exhaust =<< selectList (UpgradeControlledBy $ IdentityWithId you)
          stunIdentity you
      -- deal 2 damage to each character you control
      | otherwise = damageAllCharacters attrs (toDamage 2 FromAbility) (CharacterControlledBy $ IdentityWithId you)
