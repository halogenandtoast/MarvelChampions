module Marvel.Treachery.Treacheries.OldRivals (oldRivals, OldRivals (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

oldRivals :: TreacheryCard OldRivals
oldRivals = treachery OldRivals Cards.oldRivals

newtype OldRivals = OldRivals (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage OldRivals where
  runMessage msg t@(OldRivals attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      minions <- selectList (MinionIs Minions.gamoraNemesis)
      heroes <- selectList $ IdentityWithTitle "Gamora" <> HeroIdentity
      allies <- selectList $ AllyWithTitle "Gamora"
      let gamoras = map MinionCharacter minions <> map IdentityCharacter heroes <> map AllyCharacter allies
      case gamoras of
        [] -> gainSurge attrs
        [gamora] -> cardSpecific attrs "attack" (you, gamora)
        _ -> chooseOne you [targeting (characterRef gamora) (cardSpecific attrs "attack" (you, gamora)) | gamora <- gamoras]
      pure t
    CardSpecific (isTarget attrs -> True) "attack" (fromJSON -> Success (you, gamora)) -> do
      prohibited <- elem CannotAttack <$> getModifiers (characterRef gamora)
      stunned <- elem gamora <$> selectList (CharacterWithStatus StunnedStatus)
      -- Stun replaces an attack rather than making one. Still issue the request
      -- to discard that stun, then surge. A prohibition does not consume stun.
      unless prohibited $ case gamora of
        MinionCharacter mid -> enemyAttacks mid you
        _ -> friendlyCharacterAttacks gamora you
      when (prohibited || stunned) $ gainSurge attrs
      pure t
    _ -> OldRivals <$> runMessage msg attrs
