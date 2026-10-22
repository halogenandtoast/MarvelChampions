{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Hero.Runner (
  module X,
  module Marvel.Hero.Runner,
) where

import Marvel.Prelude

import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Types as X
import Marvel.Keyword (Keyword (Overkill))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

instance HasAbilities Hero where
  getAbilities (Hero a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability a 300 Basic (SchemeExists ThwartableScheme <> SelfMatches IdentityCanThwart) ExhaustCost Thwart
      , ability a 301 Basic canAttack ExhaustCost Attack
      ]

getModifiedAttack :: (HasGame m) => Attrs Hero -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ heroBaseAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedThwart :: (HasGame m) => Attrs Hero -> m Natural
getModifiedThwart attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unThw $ heroBaseThwart attrs) modifiers
 where
  applyModifier (ThwartModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedDefense :: (HasGame m) => Attrs Hero -> m Natural
getModifiedDefense attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unDef $ heroBaseDefense attrs) modifiers
 where
  applyModifier (DefenseModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

instance RunMessage Hero where
  runMessage msg (Hero a) = Hero <$> runMessage msg a

instance RunMessage (Attrs Hero) where
  runMessage msg a = runQueueT $ case msg of
    IdentityMessage ident (SideMessage msg') | ident == heroIdentityId a ->
      case msg' of
        -- the interrupts to making a basic attack or thwart come first: what they add to
        -- the power is added to the attack or thwart waiting for them
        Attacked -> do
          stunned <- identityMatches StunnedIdentity (heroIdentityId a)
          if stunned
            then removeStun ident
            else do
              checkWindows [W.Window W.When $ W.MadeBasicAttack ident]
              basicAttack ident 0
          pure a
        BasicAttack bonus -> do
          enemies <- selectList $ AttackableBy (IdentityWithId ident)
          dmg <- (+ bonus) <$> getModifiedAttack a
          -- an attack that gains overkill: the hero has it as a keyword while it is made
          overkill <- elem (KeywordModifier Overkill) <$> getModifiers a
          let damage = toDamage dmg (FromPlayerAttack ident) & if overkill then withOverkill else id
          chooseOnePrompted ident "$label.chooseEnemyToAttack"
            [targeting (enemyRef enemy) (dealDamage enemy a damage) | enemy <- enemies]
          checkWindows [W.Window W.After $ W.MadeBasicAttack ident]
          pure a
        Thwarted -> do
          confused <- identityMatches ConfusedIdentity (heroIdentityId a)
          if confused
            then removeConfused ident
            else do
              checkWindows [W.Window W.When $ W.MadeBasicThwart ident]
              basicThwart ident 0
          pure a
        BasicThwart bonus -> do
          schemes <- selectList ThwartableScheme
          thw <- (+ bonus) <$> getModifiedThwart a
          chooseOnePrompted ident "$label.chooseSchemeToThwart"
            [targeting (schemeTarget scheme) (thwartScheme scheme a thw) | scheme <- schemes]
          pure a
        Defended enemy -> do
          exhaust ident
          enemyDefendedBy enemy (IdentityCharacter ident)
          checkWindows [W.Window W.When $ W.HeroDefends ident enemy]
          defenseApplied ident enemy
          pure a
        DefenseApplied _ -> do
          addDefense ident =<< getModifiedDefense a
          pure a
        _ -> pure a
    _ -> pure a
   where
    -- a scheme's own ref, main or side
    schemeTarget = \case
      SchemeMainSchemeId sid -> toTarget sid
      SchemeSideSchemeId sid -> toTarget sid
