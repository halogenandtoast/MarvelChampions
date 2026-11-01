-- | Tower Defense's treacheries (The Mad Titan's Shadow).
module Marvel.Treachery.Treacheries.TowerDefense (
  proximasPower,
  ProximasPower (..),
  corvussCunning,
  CorvussCunning (..),
  boundByBlood,
  BoundByBlood (..),
  rainFire,
  RainFire (..),
) where

import Marvel.Prelude

import Marvel.Attack ()
import Marvel.Card
import Marvel.Entity
import Marvel.Environment.Environments.AvengersTower (damageAvengersTower)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

-- | When Revealed: the villain titled activates against you: he attacks your hero, or
-- schemes against your alter-ego.
activatesAgainst :: (ReverseQueue m) => Text -> IdentityId -> m ()
activatesAgainst name you = villainNamed name >>= traverse_ (`villainActivatesAgainst` you)

-- | [star] Boost: "Add the other villain's SCH and ATK to this villain's SCH and ATK for
-- this activation": the one the activation uses, ATK for an attack, SCH for a scheme. A
-- minion's activation is no villain's: nothing.
addOtherVillain :: (ReverseQueue m) => EnemyId -> m ()
addOtherVillain = \case
  EnemyVillainId v -> do
    other <- headMay . filter (/= v) <$> selectList AnyVillain
    attacking <- isJust <$> getVillainAttack v
    for_ other \o -> addBoost v =<< (if attacking then getVillainAttackValue o else getVillainSchemeValue o)
  EnemyMinionId _ -> pure ()

proximasPower :: TreacheryCard ProximasPower
proximasPower = treachery ProximasPower Cards.proximasPower

newtype ProximasPower = ProximasPower (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: Proxima Midnight activates against you.
instance RunMessage ProximasPower where
  runMessage msg t@(ProximasPower attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> t <$ activatesAgainst "Proxima Midnight" you
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> t <$ addOtherVillain enemy
    _ -> ProximasPower <$> runMessage msg attrs

corvussCunning :: TreacheryCard CorvussCunning
corvussCunning = treachery CorvussCunning Cards.corvussCunning

newtype CorvussCunning = CorvussCunning (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: Corvus Glaive activates against you.
instance RunMessage CorvussCunning where
  runMessage msg t@(CorvussCunning attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> t <$ activatesAgainst "Corvus Glaive" you
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> t <$ addOtherVillain enemy
    _ -> CorvussCunning <$> runMessage msg attrs

boundByBlood :: TreacheryCard BoundByBlood
boundByBlood = treachery BoundByBlood Cards.boundByBlood

newtype BoundByBlood = BoundByBlood (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: heal 2 damage from each villain, and give each villain a tough status
-- card. [star] Boost: the same for the active villain.
instance RunMessage BoundByBlood where
  runMessage msg t@(BoundByBlood attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      traverse_ toughen =<< selectList AnyVillain
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      traverse_ toughen =<< selectOne ActiveVillain
      pure t
    _ -> BoundByBlood <$> runMessage msg attrs
   where
    toughen v = heal v 2 >> enemyBecomesTough v

rainFire :: TreacheryCard RainFire
rainFire = treachery RainFire Cards.rainFire

newtype RainFire = RainFire (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: deal 3 damage to Avengers Tower. [star] Boost: if damage from this
-- attack defeats an ally, deal 3 damage to Avengers Tower. A boost card is there until
-- its activation ends, so an ally defeated by an attack meanwhile is by this one.
instance RunMessage RainFire where
  runMessage msg t@(RainFire attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> t <$ damageAvengersTower 3
    Boost (AllyMessage _ (AllyDefeatedByAttack _)) -> t <$ damageAvengersTower 3
    _ -> RainFire <$> runMessage msg attrs
