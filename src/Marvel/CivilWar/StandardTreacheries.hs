module Marvel.CivilWar.StandardTreacheries where

import Marvel.Prelude
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype StandardPvpTreachery = StandardPvpTreachery (Attrs Treachery `With` Maybe Natural)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

standardTreacheryDefinitions :: [CardDef]
standardTreacheryDefinitions = filter ((`elem` ["56125","56126","56127"]) . C.canonicalCode . toCardCode) C.civilWarDefinitions

standardTreacheries :: [TreacheryCard StandardPvpTreachery]
standardTreacheries = map (treachery $ \a -> StandardPvpTreachery (a `With` Nothing)) standardTreacheryDefinitions

instance IsTreachery StandardPvpTreachery where
  toTreacheryAttrs (StandardPvpTreachery (a `With` _)) = a

instance RunMessage StandardPvpTreachery where
  runMessage msg e@(StandardPvpTreachery (a `With` extra)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      case C.canonicalCode (toCardCode a) of
        "56125" -> do
          players <- asYou you getPlayers
          enemyLeaderFor you >>= traverse_ (\vid -> enemyActivationAfterCurrent vid do
            when (length players > 1) $ additionalActivationBoost vid you
            villainSchemesAgainst vid you)
        "56126" -> do
          hero <- identityMatches (IdentityWithId you <> HeroIdentity) you
          enemy <- enemyLeaderFor you
          if hero then for_ enemy (`enemyAttacks` you) else do
            friendly <- friendlyLeaderFor you
            for_ [(attacker,defender) | Just attacker <- [enemy],Just defender <- [friendly]] $ uncurry leaderAttacksLeader
        "56127" -> do
          key <- getDeckKeyFor you
          cards <- take 5 . pileDeck <$> getPile key
          lookForEnemyTeam you a cards [cardLabeled (toCardCode card) $ fromDeck key $ dealFoundEncounterCard you card | card <- cards]
          fromDeck key shuffleEncounterDeck
        _ -> pure ()
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target,C.canonicalCode (toCardCode a) == "56127" -> do
      you <- getBoostedPlayer enemy
      n <- fromIntegral . length <$> asYou you getPlayers
      pure $ StandardPvpTreachery (a `With` Just n)
    Boost (CheckWindows windows) | Just n <- extra -> do
      for_ [card | W.Window W.When (W.CountingBoostIcons card) <- windows,ecCardId card == CardId (unTreacheryId $ treacheryId a)] $ \card -> changeBoostIconCount card (+ n)
      pure e
    _ -> (\updated -> StandardPvpTreachery (updated `With` extra)) <$> runMessage msg a
