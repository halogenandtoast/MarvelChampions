-- | Tower Defense's villains (The Mad Titan's Shadow): Proxima Midnight and Corvus Glaive,
-- in play together, neither defeated while the other has hit points left.
module Marvel.Villain.Villains.TowerDefense (
  proximaMidnight1,
  proximaMidnight2,
  proximaMidnight3,
  ProximaMidnight (..),
  corvusGlaive1,
  corvusGlaive2,
  corvusGlaive3,
  CorvusGlaive (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attack
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Environment.Environments.AvengersTower (damageAvengersTower)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (Choice (RunAbility))
import Marvel.Queue (HasQueue, withQueue_)
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W

-- | "<This villain> cannot be defeated while <the other> has any hit points remaining":
-- damage takes him to 0 hit points at most, none of it excess, and he stays in play
-- ('heldUp'). The other then defeated, both are, the one held up given his own window
-- after his defeat; each advances to his next stage or, at his last, leaves play (the
-- players win with the second). Each stage defeated advances: (I) to (II), and in expert
-- play (II) to (III).
blackOrderVillain ::
  (ReverseQueue m, IsVillain a) =>
  (Attrs Villain -> a) ->
  (Natural -> Maybe (VillainCard a)) ->
  Message ->
  Attrs Villain ->
  m a ->
  m a
blackOrderVillain f nextStage msg attrs passOn = case msg of
  VillainMessage ident VillainWouldBeDefeated | ident == vid -> do
    other <- filter (/= vid) <$> selectList AnyVillain
    standing <- anyM (fmap (> 0) . getVillainHitPoints) other
    if standing
      then cancelVillainDefeat vid
      else do
        defeatVillain vid
        for_ other defeatVillain
        defeatedWith vid other
    pure $ f attrs
  VillainMessage ident VillainDefeated | ident == vid -> do
    difficulty <- getDifficulty
    case nextStage (villainStage attrs) of
      Just stage | villainStage attrs == 1 || difficulty == Expert -> do
        advanceVillain vid
        pure $ advanceVillainTo stage attrs
      _ -> passOn
  _ -> passOn
 where
  vid = villainId attrs

-- | Not defeated while the other villain has any hit points remaining.
heldUp :: (HasGame m) => Attrs Villain -> m [Modifier]
heldUp a = do
  others <- filter (/= villainId a) <$> selectList AnyVillain
  standing <- anyM (fmap (> 0) . getVillainHitPoints) others
  pure [CannotBeDefeated | standing]

-- | The villains defeated along with the one whose damage defeated him are given the
-- window after their defeat too, by the same damage, none of it excess.
defeatedWith :: (HasQueue m, HasGame m) => VillainId -> [VillainId] -> m ()
defeatedWith vid others = do
  snapshots <- for others $ \other -> (other,) <$> enemyDefeatUpgrades (EnemyVillainId other)
  withQueue_ $ \q -> case break isAfter q of
    (pre, w@(CheckWindows ws) : rest) | W.Window W.After (W.DefeatedVillain _ damage source _) : _ <- ws ->
      pre <> (w : [CheckWindows [W.Window W.After (W.DefeatedVillain o damage source 0), W.Window W.After (W.DefeatedEnemyWithUpgrades (EnemyVillainId o) defs damage source)] | (o, defs) <- snapshots]) <> rest
    _ -> q
 where
  isAfter = \case
    CheckWindows ws -> any (\case W.Window W.After (W.DefeatedVillain v _ _ _) -> v == vid; _ -> False) ws
    _ -> False

-- * Proxima Midnight

newtype ProximaMidnight = ProximaMidnight (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

proximaMidnight1, proximaMidnight2, proximaMidnight3 :: VillainCard ProximaMidnight
proximaMidnight1 = villain ProximaMidnight Cards.proximaMidnight1 (Sch 1) (Atk 2) (HP $ PerPlayer 9)
proximaMidnight2 = villainWith ProximaMidnight Cards.proximaMidnight2 (Sch 1) (Atk 3) (HP $ PerPlayer 12) (stageL .~ 2)
proximaMidnight3 = villainWith ProximaMidnight Cards.proximaMidnight3 (Sch 2) (Atk 3) (HP $ PerPlayer 15) (stageL .~ 3)

instance HasModifiersFor ProximaMidnight where
  getModifiersFor _ (VillainRef v) (ProximaMidnight a) | v == villainId a = heldUp a
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Interrupt: when Proxima Midnight attacks you, choose to either deal 1
-- damage to Avengers Tower, or she gets +2 ATK for this attack.
instance HasAbilities ProximaMidnight where
  getAbilities (ProximaMidnight a) =
    [ windowAbility a 1 (EnemyAttacked When (EnemyWithId $ EnemyVillainId $ villainId a) AnyIdentity) ForcedInterrupt NoCost $
        RunAbility (toTarget a) 1
    ]

instance RunMessage ProximaMidnight where
  runMessage msg (ProximaMidnight attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [you | W.EnemyAttack _ you <- windows] \you ->
        chooseOne you
          [ labeled (countLabel "damageAvengersTower" 1) (damageAvengersTower 1)
          , labeled (countLabel "proximaGetsAttack" 2) (cardSpecific attrs "attackGets2" ())
          ]
      pure $ ProximaMidnight attrs
    CardSpecific (isTarget attrs -> True) "attackGets2" _ ->
      pure . ProximaMidnight $ attrs & attackingL . _Just %~ (\attack -> attack {attackChange = attackChange attack + 2})
    _ -> blackOrderVillain ProximaMidnight next msg attrs (ProximaMidnight <$> runMessage msg attrs)
   where
    next = \case
      1 -> Just proximaMidnight2
      2 -> Just proximaMidnight3
      _ -> Nothing

-- * Corvus Glaive

newtype CorvusGlaive = CorvusGlaive (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

corvusGlaive1, corvusGlaive2, corvusGlaive3 :: VillainCard CorvusGlaive
corvusGlaive1 = villain CorvusGlaive Cards.corvusGlaive1 (Sch 2) (Atk 1) (HP $ PerPlayer 8)
corvusGlaive2 = villainWith CorvusGlaive Cards.corvusGlaive2 (Sch 2) (Atk 2) (HP $ PerPlayer 11) (stageL .~ 2)
corvusGlaive3 = villainWith CorvusGlaive Cards.corvusGlaive3 (Sch 3) (Atk 2) (HP $ PerPlayer 14) (stageL .~ 3)

instance HasModifiersFor CorvusGlaive where
  getModifiersFor _ (VillainRef v) (CorvusGlaive a) | v == villainId a = heldUp a
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Response (as printed, MTS rulebook p.12): after Corvus Glaive makes an
-- undefended attack, discard the top card of the encounter deck → deal 1 damage to
-- Avengers Tower for each boost icon on that card.
instance HasAbilities CorvusGlaive where
  getAbilities (CorvusGlaive a) =
    [ limitedWindowAbility
        a
        1
        (EnemyAttacked After self AnyIdentity)
        ForcedResponse
        (EnemyExists $ self <> UndefendedEnemy)
        NoCost
        $ RunAbility (toTarget a) 1
    ]
   where
    self = EnemyWithId $ EnemyVillainId $ villainId a

instance RunMessage CorvusGlaive where
  runMessage msg (CorvusGlaive attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs ("corvusGlaive" :: Text))
      pure $ CorvusGlaive attrs
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success (key :: Text)) counts | key == "corvusGlaive" -> do
      damageAvengersTower (sum $ map snd counts)
      pure $ CorvusGlaive attrs
    _ -> blackOrderVillain CorvusGlaive next msg attrs (CorvusGlaive <$> runMessage msg attrs)
   where
    next = \case
      1 -> Just corvusGlaive2
      2 -> Just corvusGlaive3
      _ -> Nothing
