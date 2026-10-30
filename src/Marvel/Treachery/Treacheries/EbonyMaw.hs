-- | The treacheries of Ebony Maw's set and the Black Order modular set (The Mad
-- Titan's Shadow).
module Marvel.Treachery.Treacheries.EbonyMaw (
  agentOfThanos,
  AgentOfThanos (..),
  channelingTrance,
  ChannelingTrance (..),
  bloodToSpare,
  BloodToSpare (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Environment.Environments.EbonyMawSpells (removeInvocationCounters, spellEnvironment)
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

agentOfThanos :: TreacheryCard AgentOfThanos
agentOfThanos = treachery AgentOfThanos Cards.agentOfThanos

newtype AgentOfThanos = AgentOfThanos (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed (Alter-Ego): 1 threat on the main scheme for each Spell environment in
-- your play area. (Hero): 1 damage to your hero for each. None placed or taken, this
-- card gains surge.
instance RunMessage AgentOfThanos where
  runMessage msg t@(AgentOfThanos attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      x <- selectListCount (EnvironmentInPlayAreaOf you)
      if x == 0
        then pure . AgentOfThanos $ attrs & surgeL .~ True
        else do
          hero <- identityMatches HeroIdentity you
          if hero
            then damageIdentity you attrs (toDamage x FromTreachery)
            else placeThreatOnEach attrs x EncounterMainScheme
          pure t
    _ -> AgentOfThanos <$> runMessage msg attrs

channelingTrance :: TreacheryCard ChannelingTrance
channelingTrance = treachery ChannelingTrance Cards.channelingTrance

newtype ChannelingTrance = ChannelingTrance (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: remove 1 invocation counter from each Spell environment in your play
-- area. With none there, discard cards from the top of the encounter deck until a Spell
-- environment is discarded, and put it into play in your play area.
instance RunMessage ChannelingTrance where
  runMessage msg t@(ChannelingTrance attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      none <- selectNone (EnvironmentInPlayAreaOf you)
      if none then discardUntilIntoPlay you spellEnvironment else removeInvocationCounters you
      pure t
    _ -> ChannelingTrance <$> runMessage msg attrs

bloodToSpare :: TreacheryCard BloodToSpare
bloodToSpare = treachery BloodToSpare Cards.bloodToSpare

newtype BloodToSpare = BloodToSpare (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: each minion engaged with a player activates against that player (it
-- attacks a hero, schemes against an alter-ego). Then each player not engaged with a
-- minion searches the encounter deck and discard pile for a Black Order minion and puts
-- it into play engaged with them. Shuffle the encounter deck.
instance RunMessage BloodToSpare where
  runMessage msg t@(BloodToSpare attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      -- player by player, each looked at as the previous player's activations left the
      -- game: a player eliminated by them has no minions activate against them
      players <- getPlayers
      for_ players \p -> cardSpecific attrs "bloodToSpareActivate" p
      -- who is engaged with a minion once those activations have resolved
      cardSpecific attrs "bloodToSpareSearch" ()
      pure t
    CardSpecific (isTarget attrs -> True) "bloodToSpareActivate" (fromJSON -> Success p) -> do
      minions <- selectList (MinionEngagedWith $ IdentityWithId p)
      for_ minions \m -> cardSpecific attrs "bloodToSpareMinion" (p, m)
      pure t
    -- each minion's activation as the last left the game: its player still in it, the
    -- minion still engaged with them
    CardSpecific (isTarget attrs -> True) "bloodToSpareMinion" (fromJSON -> Success (p, m)) -> do
      inGame <- elem p <$> getPlayers
      engaged <- elem m <$> selectList (MinionEngagedWith $ IdentityWithId p)
      when (inGame && engaged) do
        hero <- identityMatches HeroIdentity p
        if hero then enemyAttacks m p else minionSchemes m
      pure t
    CardSpecific (isTarget attrs -> True) "bloodToSpareSearch" _ -> do
      searchers <- filterM (selectNone . MinionEngagedWith . IdentityWithId) =<< getPlayers
      for_ searchers \p -> searchAndPutIntoPlayEncounterCard p (CardWithType MinionType <> CardWithTrait BlackOrder)
      -- each search shuffles the deck
      when (null searchers) shuffleEncounterDeck
      pure t
    _ -> BloodToSpare <$> runMessage msg attrs
