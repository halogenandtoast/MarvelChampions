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
import Marvel.Keyword (Keyword (Overkill, Piercing))
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
      [ ability a 300 Basic (SchemeExists (SchemeAHeroCanBasicThwart $ heroIdentityId $ toHeroAttrs a) <> SelfMatches IdentityCanThwart) ExhaustCost Thwart
      , ability a 301 Basic (canAttack <> SelfMatches IdentityCanBasicAttack) ExhaustCost Attack
      ]

-- | All the modifiers are added and taken off first, then a value below zero is zero (RR
-- Modifiers).
modifiedStat :: Natural -> (Modifier -> Integer) -> [Modifier] -> Natural
modifiedStat base f modifiers = fromInteger $ max 0 (toInteger base + sum (map f modifiers))

getModifiedAttack :: (HasGame m) => Attrs Hero -> m Natural
getModifiedAttack attrs = modifiedStat (unAtk $ heroBaseAttack attrs) change <$> getModifiers attrs
 where
  change (AttackModifier n) = toInteger n
  change (AttackPenalty n) = negate (toInteger n)
  change _ = 0

-- | The ATK during an attack against this enemy. Conditional modifiers add to the
-- ordinary statistic, so a weapon's +2 instead of +1 is baseline +1, contextual +1.
getModifiedAttackAgainst :: (HasGame m) => Attrs Hero -> EnemyId -> m Natural
getModifiedAttackAgainst attrs enemy = do
  normal <- getModifiedAttack attrs
  modifiers <- getModifiers attrs
  pure $ normal + sum [n | AttackModifierAgainst target n <- modifiers, target == enemy]

getModifiedDefenseAgainst :: (HasGame m) => Attrs Hero -> EnemyId -> m Natural
getModifiedDefenseAgainst attrs enemy = do
  normal <- getModifiedDefense attrs
  modifiers <- getModifiers attrs
  pure $ normal + sum [n | DefenseModifierAgainst target n <- modifiers, target == enemy]

getModifiedThwart :: (HasGame m) => Attrs Hero -> m Natural
getModifiedThwart attrs = modifiedStat (unThw $ heroBaseThwart attrs) change <$> getModifiers attrs
 where
  change (ThwartModifier n) = toInteger n
  change (ThwartPenalty n) = negate (toInteger n)
  change _ = 0

getModifiedDefense :: (HasGame m) => Attrs Hero -> m Natural
getModifiedDefense attrs = modifiedStat (unDef $ heroBaseDefense attrs) change <$> getModifiers attrs
 where
  change (DefenseModifier n) = toInteger n
  change (DefensePenalty n) = negate (toInteger n)
  change _ = 0

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
              replaced <- replaceHeroAttackWithDiscardAndStun ident
              unless replaced do
                checkWindows [W.Window W.When $ W.MadeBasicAttack ident]
                basicAttack ident 0
          pure a
        BasicAttack bonus -> do
          enemies <- selectList $ AttackableBy (IdentityWithId ident)
          damage <- attackDamage ident bonus
          modifiers <- getModifiers a
          let
            -- damage that may be divided among enemies, each of them attacked
            divided =
              [ labeled "$label.divideDamage" (dealDividedDamage ident a damage (AttackableBy (IdentityWithId ident)))
              | DividesBasicAttack `elem` modifiers
              , length enemies > 1
              ]
          -- the target chosen, the interrupts to attacking it (Godslayer), then the damage
          chooseOnePrompted ident "$label.chooseEnemyToAttack" $
            [ targeting (enemyRef enemy) do
                checkWindows [W.Window W.When $ W.MadeBasicAttackAgainst ident enemy]
                basicAttackOn ident enemy bonus
                -- "after a hero makes a basic attack against <enemy>" (Thanos's Armor)
                checkWindows [W.Window W.After $ W.MadeBasicAttackAgainst ident enemy]
            | enemy <- enemies
            ]
              <> divided
          checkWindows [W.Window W.After $ W.MadeBasicAttack ident]
          pure a
        BasicAttackOn enemy bonus -> do
          damage <- attackDamageAgainst ident enemy bonus
          dealDamage enemy a damage
          checkWindows [W.Window W.After $ W.PlayerAttackResolved (toSource a) ident [enemy]]
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
          schemes <- selectList (SchemeAHeroCanBasicThwart ident)
          thw <- (+ bonus) <$> getModifiedThwart a
          atk <- (+ bonus) <$> getModifiedAttack a
          -- a scheme thwarted with ATK instead of THW, as The Red House allows
          choices <- for schemes \scheme -> do
            withAttack <- elem ThwartWithAttack <$> getModifiers (schemeTarget scheme)
            pure $ targeting (schemeTarget scheme) $
              if withAttack && atk /= thw
                then
                  chooseOne ident
                    [ labeled "$label.thwartWithThw" (basicThwartOf scheme a thw)
                    , labeled "$label.thwartWithAtk" (basicThwartOf scheme a atk)
                    ]
                else basicThwartOf scheme a thw
          -- threat that may be divided among schemes, each of them thwarted
          divides <- elem DividesBasicThwart <$> getModifiers a
          let divided = [labeled "$label.divideThreat" (basicThwartDivided ident a thw) | divides, length schemes > 1]
          chooseOnePrompted ident "$label.chooseSchemeToThwart" (choices <> divided)
          checkWindows [W.Window W.After $ W.MadeBasicThwart ident]
          pure a
        Defended enemy -> do
          forbidden <- elem AttacksCannotBeDefended <$> getModifiers (enemyRef enemy)
          unless forbidden do
            exhaust ident
            enemyDefendedBy enemy (IdentityCharacter ident)
            checkWindows [W.Window W.When $ W.HeroDefends ident enemy, W.Window W.When $ W.MadeBasicDefense ident enemy]
            defenseApplied ident enemy
          pure a
        -- A card may declare the basic defender without exhausting it. Its DEF is
        -- applied once, after the same defense window as an ordinary basic defense.
        DefendedWithoutExhausting bonus enemy -> do
          forbidden <- elem AttacksCannotBeDefended <$> getModifiers (enemyRef enemy)
          unless forbidden do
            enemyDefendedBy enemy (IdentityCharacter ident)
            checkWindows [W.Window W.When $ W.HeroDefends ident enemy, W.Window W.When $ W.MadeBasicDefense ident enemy]
            identityStep ident (SideMessage $ DefenseApplied bonus enemy)
          pure a
        DefenseApplied bonus enemy -> do
          usingAttack <- usesAttackForDefense ident enemy
          if usingAttack
            then addDefense ident =<< getModifiedAttack a
            else addDefense ident . (+ bonus) =<< getModifiedDefenseAgainst a enemy
          pure a
        DefenseUsingAttackApplied _ _ -> do
          -- DEF modifiers, including bonuses in the pending defense, are ignored:
          -- The Best Defense uses ATK instead of DEF, not ATK plus bonus DEF.
          addDefense ident =<< getModifiedAttack a
          pure a
        _ -> pure a
    _ -> pure a
   where
    -- an attack that gains overkill or piercing: the hero has it as a keyword while it is
    -- made
    attackDamage ident bonus = do
      dmg <- (+ bonus) <$> getModifiedAttack a
      modifiers <- getModifiers a
      let gains k f = if KeywordModifier k `elem` modifiers then f else id
      pure $ toDamage dmg (FromPlayerAttack ident) & gains Overkill withOverkill & gains Piercing withPiercing
    attackDamageAgainst ident enemy bonus = do
      dmg <- (+ bonus) <$> getModifiedAttackAgainst a enemy
      modifiers <- getModifiers a
      let gains k f = if KeywordModifier k `elem` modifiers then f else id
      pure $ toDamage dmg (FromPlayerAttack ident) & gains Overkill withOverkill & gains Piercing withPiercing
    -- a scheme's own ref, main or side
    schemeTarget = \case
      SchemeMainSchemeId sid -> toTarget sid
      SchemeSideSchemeId sid -> toTarget sid
