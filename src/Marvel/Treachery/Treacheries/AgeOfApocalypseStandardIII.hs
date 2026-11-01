module Marvel.Treachery.Treacheries.AgeOfApocalypseStandardIII where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Marvel.Prelude
import Marvel.AgeOfApocalypse.StandardIII
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.Game.Source
import Marvel.EncounterSet (getNemesisSet)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query

newtype StandardIIITreachery = StandardIIITreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

darkDesigns, sinisterStrike, evilAlliance, nowhereIsSafe :: TreacheryCard StandardIIITreachery
darkDesigns = treachery StandardIIITreachery Cards.darkDesigns
sinisterStrike = treachery StandardIIITreachery Cards.sinisterStrike
evilAlliance = treachery StandardIIITreachery Cards.evilAlliance
nowhereIsSafe = treachery StandardIIITreachery Cards.nowhereIsSafe

instance RunMessage StandardIIITreachery where
  runMessage msg e@(StandardIIITreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      if toCardCode a == "45078" then do
        sets <- List.nub . map getNemesisSet <$> getStartingHeroCodes
        let defs = filter (\d -> cdEncounterSet d `elem` map Just sets && Minions.isNemesisMinion d) (HashMap.elems Minions.allMinions)
        minions <- concatMapM (selectList . MinionIs) defs
        if null minions then addPursuit you 3 else traverse_ (`minionActivatesAgainst` you) minions
      else do
        addPursuit you 1
        cardSpecific a "afterPursuit" you
      pure e
    CardSpecific (isTarget a -> True) "afterPursuit" (fromJSON -> Success you) -> do
      remains <- hasPursuit
      when remains $ case toCardCode a of
        "45076" -> selectOne ActiveVillain >>= traverse_ (`villainSchemesAgainst` you)
        "45077" -> do
          hero <- selectAny (IdentityWithId you <> HeroIdentity)
          if hero then selectOne ActiveVillain >>= traverse_ (`enemyAttacks` you) else gainSurge a
        "45079" -> do
          upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
          supports <- selectList (SupportControlledBy $ IdentityWithId you)
          chooseAndDiscardControlled you (map UpgradeRef upgrades <> map SupportRef supports)
        _ -> pure ()
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a `elem` ["45076", "45078", "45079"] -> do
      you <- getBoostedPlayer enemy
      enemyActivationAfterCurrent enemy (addPursuit you 1)
      pure e
    _ -> StandardIIITreachery <$> runMessage msg a
