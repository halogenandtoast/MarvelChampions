-- | The Band of Badoon modular set's minions (The Galaxy's Most Wanted), and the
-- Brotherhood of Badoon's Badoon Engineer.
module Marvel.Minion.Minions.BandOfBadoon (
  badoonAssassin,
  BadoonAssassin (..),
  badoonGrunt,
  BadoonGrunt (..),
  badoonLieutenant,
  BadoonLieutenant (..),
  badoonSentry,
  BadoonSentry (..),
  badoonWarlord,
  BadoonWarlord (..),
  badoonEngineer,
  BadoonEngineer (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Environment.Environments.BadoonShip (chargeUp)
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

-- | Whether the activation under way is an attack.
isAttacking :: (HasGame m) => EnemyId -> m Bool
isAttacking enemy = selectAny $ EnemyWithId enemy <> AttackingEnemy

engages :: Attrs Minion -> WindowMatcher
engages a = EngagesMinion After AnyIdentity (MinionWithId $ minionId a)

badoonAssassin :: MinionCard BadoonAssassin
badoonAssassin = minion (BadoonAssassin . (`With` Meta False)) Cards.badoonAssassin (Sch 1) (Atk 1) (HP 1)

-- | Whether the attack under way is the one it makes as it engages, with +2 ATK.
newtype Meta = Meta {engaging :: Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype BadoonAssassin = BadoonAssassin (Attrs Minion `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsMinion BadoonAssassin where
  toMinionAttrs (BadoonAssassin (attrs `With` _)) = attrs
  overMinionAttrs f (BadoonAssassin (attrs `With` meta)) = BadoonAssassin (f attrs `With` meta)

instance IsRef BadoonAssassin where
  toRef = toRef . toMinionAttrs

instance HasModifiersFor BadoonAssassin where
  getModifiersFor _ (MinionRef mid) (BadoonAssassin (attrs `With` meta))
    | mid == minionId attrs && engaging meta = pure [AttackModifier 2]
  getModifiersFor _ _ _ = pure []

-- | Forced Response: after Badoon Assassin engages your hero, it attacks you with +2 ATK.
instance HasAbilities BadoonAssassin where
  getAbilities (BadoonAssassin (a `With` _)) =
    [ windowAbility a 1 (EngagesMinion After HeroIdentity (MinionWithId $ minionId a)) ForcedResponse NoCost $
        runAbility a 1
    ]

-- | [star] Boost: if this activation is an attack, the attack gains overkill, piercing
-- and ranged.
instance RunMessage BadoonAssassin where
  runMessage msg e@(BadoonAssassin (attrs `With` meta)) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      enemyAttacks (minionId attrs) (minionEngagedIdentity attrs)
      pure $ BadoonAssassin (attrs `With` Meta True)
    MinionMessage mid MinionEndAttack | mid == minionId attrs ->
      BadoonAssassin . (`With` Meta False) <$> runMessage msg attrs
    -- stunned, it does not attack: its stun is removed instead
    MinionMessage mid (MinionAttacks _) | mid == minionId attrs && minionStunned attrs ->
      BadoonAssassin . (`With` Meta False) <$> runMessage msg attrs
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      whenM (isAttacking enemy) $ for_ [Overkill, Piercing, Ranged] (attackGains enemy)
      pure e
    _ -> BadoonAssassin . (`With` meta) <$> runMessage msg attrs

badoonGrunt :: MinionCard BadoonGrunt
badoonGrunt = minion BadoonGrunt Cards.badoonGrunt (Sch 2) (Atk 2) (HP 2)

newtype BadoonGrunt = BadoonGrunt (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Response: after Badoon Grunt engages you, if there are no other minions
-- engaged with you, deal yourself 1 facedown encounter card.
instance HasAbilities BadoonGrunt where
  getAbilities (BadoonGrunt a) = [windowAbility a 1 (engages a) ForcedResponse NoCost $ runAbility a 1]

-- | [star] Boost: put Badoon Grunt into play engaged with you.
instance RunMessage BadoonGrunt where
  runMessage msg e@(BadoonGrunt attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let you = minionEngagedIdentity attrs
      others <- selectAny $ MinionEngagedWith (IdentityWithId you) <> NotMinion (MinionWithId $ minionId attrs)
      unless others $ dealEncounterCard you
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      e <$ putBoostIntoPlay target you
    _ -> BadoonGrunt <$> runMessage msg attrs

-- | Patrol, its keyword.
badoonLieutenant :: MinionCard BadoonLieutenant
badoonLieutenant = minion BadoonLieutenant Cards.badoonLieutenant (Sch 2) (Atk 2) (HP 6)

newtype BadoonLieutenant = BadoonLieutenant (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Boost: if this activation is a scheme, this card gets +2 boost icons for this
-- activation.
instance RunMessage BadoonLieutenant where
  runMessage msg e@(BadoonLieutenant attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      attacking <- isAttacking enemy
      unless attacking $ addEnemyBoost enemy 2
      pure e
    _ -> BadoonLieutenant <$> runMessage msg attrs

-- | Retaliate 1, its keyword.
badoonSentry :: MinionCard BadoonSentry
badoonSentry = minion BadoonSentry Cards.badoonSentry (Sch 1) (Atk 1) (HP 5)

newtype BadoonSentry = BadoonSentry (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Boost: give the villain a tough status card. If the villain already has one,
-- this card gets +2 boost icons for this activation.
instance RunMessage BadoonSentry where
  runMessage msg e@(BadoonSentry attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      selectOne ActiveVillain >>= traverse_ \villain -> do
        tough <- selectAny (VillainWithId villain <> VillainWithToughStatus)
        if tough then addEnemyBoost enemy 2 else enemyBecomesTough villain
      pure e
    _ -> BadoonSentry <$> runMessage msg attrs

badoonWarlord :: MinionCard BadoonWarlord
badoonWarlord = minion BadoonWarlord Cards.badoonWarlord (Sch 1) (Atk 3) (HP 4)

newtype BadoonWarlord = BadoonWarlord (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Badoon Warlord's attacks gain overkill.
instance HasModifiersFor BadoonWarlord where
  getModifiersFor _ (MinionRef mid) (BadoonWarlord a) | mid == minionId a = pure [KeywordModifier Overkill]
  getModifiersFor _ _ _ = pure []

-- | [star] Boost: if this activation is an attack, this card gets +2 boost icons for this
-- activation.
instance RunMessage BadoonWarlord where
  runMessage msg e@(BadoonWarlord attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      whenM (isAttacking enemy) $ addEnemyBoost enemy 2
      pure e
    _ -> BadoonWarlord <$> runMessage msg attrs

badoonEngineer :: MinionCard BadoonEngineer
badoonEngineer = minion BadoonEngineer Cards.badoonEngineer (Sch 2) (Atk 1) (HP 3)

newtype BadoonEngineer = BadoonEngineer (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Response: after Badoon Engineer engages you or activates against you
-- (attacks, or schemes), Charge Up.
instance HasAbilities BadoonEngineer where
  getAbilities (BadoonEngineer a) =
    [ windowAbility
        a
        1
        ( WindowOneOf
            [ engages a
            , EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity
            , MinionHasSchemed After (MinionWithId $ minionId a)
            ]
        )
        ForcedResponse
        NoCost
        $ runAbility a 1
    ]

-- | [star] Boost: Charge Up.
instance RunMessage BadoonEngineer where
  runMessage msg e@(BadoonEngineer attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ chargeUp
    Boost (RevealedAsBoost target _) | isTarget attrs target -> e <$ chargeUp
    _ -> BadoonEngineer <$> runMessage msg attrs
