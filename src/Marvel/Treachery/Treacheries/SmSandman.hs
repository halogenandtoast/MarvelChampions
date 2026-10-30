module Marvel.Treachery.Treacheries.SmSandman where

import Marvel.Prelude
import Marvel.Card
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Entity
import Marvel.Environment.Environments.SmSandman
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

data SandmanTreacheryMeta = SandmanTreacheryMeta
  { sandslidePlayer :: Maybe IdentityId
  , boostedAttack :: Maybe (VillainId, IdentityId)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SmSandmanTreachery = SmSandmanTreachery (Attrs Treachery `With` SandmanTreacheryMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery SmSandmanTreachery where
  toTreacheryAttrs (SmSandmanTreachery (attrs `With` _)) = attrs

sandmanTreachery :: Attrs Treachery -> SmSandmanTreachery
sandmanTreachery = SmSandmanTreachery . (`With` SandmanTreacheryMeta Nothing Nothing)

smSandslide, smSandStorm, smSandSmash :: TreacheryCard SmSandmanTreachery
smSandslide = treachery sandmanTreachery Cards.smSandslide
smSandStorm = treachery sandmanTreachery Cards.smSandStorm
smSandSmash = treachery sandmanTreachery Cards.smSandSmash

instance RunMessage SmSandmanTreachery where
  runMessage msg e@(SmSandmanTreachery (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> reveal you
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27070" -> reveal =<< getBoostedPlayer enemy
    Boost (WithDiscarded (isTarget attrs -> True) FromEncounterDeck cards) | toCardCode attrs == "27070" -> do
      when (any ((== Just EncounterSet.Sandman) . cdEncounterSet . getCardDef) cards) $
        traverse_ stunIdentity (sandslidePlayer meta)
      pure e
    WithDiscarded (isTarget attrs -> True) FromEncounterDeck cards | toCardCode attrs == "27070" -> do
      when (any ((== Just EncounterSet.Sandman) . cdEncounterSet . getCardDef) cards) $
        traverse_ stunIdentity (sandslidePlayer meta)
      pure e
    CheckWindows windows
      | Just (vid, you) <- boostedAttack meta
      , W.Window W.When (W.EnemyAttack (EnemyVillainId vid) you) `elem` windows -> do
          changeAttack vid 1
          pure $ SmSandmanTreachery (attrs `With` meta {boostedAttack = Nothing})
    VillainMessage vid VillainEndAttack | Just (other, _) <- boostedAttack meta, vid == other ->
      SmSandmanTreachery . (`With` meta {boostedAttack = Nothing}) <$> runMessage msg attrs
    _ -> SmSandmanTreachery . (`With` meta) <$> runMessage msg attrs
   where
    reveal you = case toCardCode attrs of
      "27070" -> do
        cityStreets >>= traverse_ (`placeCounters` 2)
        surgingSandsFor attrs
        pure $ SmSandmanTreachery (attrs `With` meta {sandslidePlayer = Just you})
      "27071" -> do
        n <- sandCounters
        if n == 0
          then do
            cityStreets >>= traverse_ (`placeCounters` 3)
            removeFromPlay attrs
            shuffleIntoEncounterDeck [MkEncounterCard (CardId $ unTreacheryId $ treacheryId attrs) $ getCardDef attrs]
          else dealIndirectDamageAmongPlayers attrs n
        pure e
      _ -> do
        hero <- identityMatches HeroIdentity you
        if hero
          then do
            villain <- selectOne ActiveVillain
            for_ villain (`enemyAttacks` you)
            pure $ SmSandmanTreachery (attrs `With` meta {boostedAttack = (,you) <$> villain})
          else do
            surgingSands
            gainSurge attrs
            pure e
