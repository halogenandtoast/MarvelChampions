module Marvel.Treachery.Treacheries.TricksterTakeover where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.TricksterTakeover.Helpers
import Marvel.Treachery.Types

newtype TTTreachery = TTTreachery (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsTreachery

tt55023 :: TreacheryCard TTTreachery
tt55023 = treachery TTTreachery Cards.tt55023

tt55024 :: TreacheryCard TTTreachery
tt55024 = treachery TTTreachery Cards.tt55024

tt55025 :: TreacheryCard TTTreachery
tt55025 = treachery TTTreachery Cards.tt55025

tt55026 :: TreacheryCard TTTreachery
tt55026 = treachery TTTreachery Cards.tt55026

tt55049 :: TreacheryCard TTTreachery
tt55049 = treachery TTTreachery Cards.tt55049

tt55050 :: TreacheryCard TTTreachery
tt55050 = treachery TTTreachery Cards.tt55050

tt55051 :: TreacheryCard TTTreachery
tt55051 = treachery TTTreachery Cards.tt55051

instance RunMessage TTTreachery where
  runMessage msg e@(TTTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      case toCardCode a of
        "55023" -> alternatives you
          ("$label.ttRevealMinion", searchAndRevealEncounterCard you $ CardWithType MinionType)
          ("$label.ttRevealSideScheme", searchAndRevealEncounterCard you $ CardWithType SideSchemeType)
        "55024" -> alternatives you
          ("$label.ttStunAndCharm", stunIdentity you >> charm you)
          ("$label.ttConfuseAndCharm", confuseIdentity you >> charm you)
        "55025" -> alternatives you
          ("$label.villainSchemes", activate you False)
          ("$label.villainAttacks", activate you True)
        "55026" -> do
          before <- getIdentityDamage you
          dealIndirectDamage you a 3
          cardSpecific a "ttShards" (you, before)
        "55049" -> do
          hard <- expert
          when hard $ mainThreat a 1
          cards <- filter (cardMatch $ CardWithType MinionType) <$> getEncounterDiscard
          if null cards then mainThreat a 3 else chooseOne you [cardLabeled (toCardCode c) (revealFoundEncounterCard you c) | c <- cards]
        "55050" -> do
          selectOne ActiveVillain >>= traverse_ (`removeCounters` 1)
          hard <- expert
          damageIdentity you a (toDamage (if hard then 3 else 2) FromTreachery)
        "55051" -> do
          cardSpecific GameRef "ttSwapAvatar" False
          cardSpecific a "ttStories" you
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "ttShards" (fromJSON -> Success (you, before :: Natural)) -> do
      present <- selectAny (IdentityWithId you)
      when present $ do
        after <- getIdentityDamage you
        when (after > before) $ charm you
      pure e
    CardSpecific (isTarget a -> True) "ttStories" (fromJSON -> Success you) -> do
      villain <- selectOne ActiveVillain
      for_ villain $ \v -> do
        code <- getsGame $ fmap toCardCode . (`cardInPlay` VillainRef v)
        activate you (code `elem` [Just "55029a", Just "55032a"])
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "55026" -> charm you
        "55049" -> selectList (CharacterControlledBy $ IdentityWithId you) >>= mainThreat a . fromIntegral . length
        "55050" -> selectList (CharacterControlledBy $ IdentityWithId you) >>= dealIndirectDamage you a . fromIntegral . length
        _ -> pure ()
      pure e
    _ -> TTTreachery <$> runMessage msg a
   where
    activate you attack = selectOne ActiveVillain >>= traverse_ (\v -> do
      hard <- expert
      when hard $ enemyActivationBonus v 1
      if attack then enemyAttacks v you else villainSchemesAgainst v you)
    alternatives you (label1, action1) (label2, action2) = do
      both <- enthralled you
      if both then chooseOne you [labeled label1 (action1 >> action2), labeled label2 (action2 >> action1)]
        else chooseOne you [labeled label1 action1, labeled label2 action2]
