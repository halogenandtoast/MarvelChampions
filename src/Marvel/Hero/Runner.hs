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
      [ ability a 300 Basic (SchemeExists ThwartableScheme <> SelfMatches IdentityCanThwart) ExhaustCost Thwart
      , ability a 301 Basic canAttack ExhaustCost Attack
      ]

getModifiedAttack :: (HasGame m) => Attrs Hero -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ heroBaseAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier (AttackPenalty n) = subtractNatural n
  applyModifier _ = id

getModifiedThwart :: (HasGame m) => Attrs Hero -> m Natural
getModifiedThwart attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unThw $ heroBaseThwart attrs) modifiers
 where
  applyModifier (ThwartModifier n) = max 0 . (+ fromIntegral n)
  applyModifier (ThwartPenalty n) = subtractNatural n
  applyModifier _ = id

getModifiedDefense :: (HasGame m) => Attrs Hero -> m Natural
getModifiedDefense attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unDef $ heroBaseDefense attrs) modifiers
 where
  applyModifier (DefenseModifier n) = max 0 . (+ fromIntegral n)
  applyModifier (DefensePenalty n) = subtractNatural n
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
          -- an attack that gains overkill or piercing: the hero has it as a keyword while
          -- it is made
          modifiers <- getModifiers a
          let
            gains k f = if KeywordModifier k `elem` modifiers then f else id
            damage = toDamage dmg (FromPlayerAttack ident) & gains Overkill withOverkill & gains Piercing withPiercing
            -- damage that may be divided among enemies, each of them attacked
            divided =
              [ labeled "$label.divideDamage" (dealDividedDamage ident a damage (AttackableBy (IdentityWithId ident)))
              | DividesBasicAttack `elem` modifiers
              , length enemies > 1
              ]
          chooseOnePrompted ident "$label.chooseEnemyToAttack" $
            [targeting (enemyRef enemy) (dealDamage enemy a damage) | enemy <- enemies] <> divided
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
          pure a
        Defended enemy -> do
          exhaust ident
          enemyDefendedBy enemy (IdentityCharacter ident)
          checkWindows [W.Window W.When $ W.HeroDefends ident enemy]
          defenseApplied ident enemy
          pure a
        DefenseApplied bonus _ -> do
          addDefense ident . (+ bonus) =<< getModifiedDefense a
          pure a
        _ -> pure a
    _ -> pure a
   where
    -- a scheme's own ref, main or side
    schemeTarget = \case
      SchemeMainSchemeId sid -> toTarget sid
      SchemeSideSchemeId sid -> toTarget sid
