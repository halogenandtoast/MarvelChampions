{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Hero.Runner
  ( module X
  , module Marvel.Hero.Runner
  ) where

import Marvel.Prelude

import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Types as X
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window qualified as W

instance HasAbilities Hero where
  getAbilities (Hero a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability a 300 Basic (SchemeExists ThwartableScheme) ExhaustCost Thwart
      , ability a 301 Basic (EnemyExists AttackableEnemy) ExhaustCost Attack
      ]

getModifiedAttack :: HasGame m => Attrs Hero -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ heroBaseAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedThwart :: HasGame m => Attrs Hero -> m Natural
getModifiedThwart attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unThw $ heroBaseThwart attrs) modifiers
 where
  applyModifier (ThwartModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedDefense :: HasGame m => Attrs Hero -> m Natural
getModifiedDefense attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unDef $ heroBaseDefense attrs) modifiers
 where
  applyModifier (DefenseModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

damageChoice :: Attrs Hero -> Damage -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [ DamageEnemy (VillainTarget vid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.IdentityAttack (heroIdentityId attrs) (EnemyVillainId vid)
          ]
      ]
    ]
  EnemyMinionId mid -> TargetLabel
    (MinionTarget mid)
    [ DamageEnemy (MinionTarget mid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.IdentityAttack (heroIdentityId attrs) (EnemyMinionId mid)
          ]
      ]
    ]

thwartChoice :: Attrs Hero -> Natural -> SchemeId -> Choice
thwartChoice attrs thw = \case
  SchemeMainSchemeId vid -> TargetLabel
    (MainSchemeTarget vid)
    [ThwartScheme (MainSchemeTarget vid) (toSource attrs) thw]
  SchemeSideSchemeId sid -> TargetLabel
    (SideSchemeTarget sid)
    [ThwartScheme (SideSchemeTarget sid) (toSource attrs) thw]

instance RunMessage Hero where
  runMessage msg (Hero a) = Hero <$> runMessage msg a

instance RunMessage (Attrs Hero) where
  runMessage msg a = case msg of
    IdentityMessage ident (SideMessage msg') | ident == heroIdentityId a ->
      case msg' of
        Attacked -> do
          stunned <- identityMatches StunnedIdentity (heroIdentityId a)
          if not stunned
            then do
              enemies <- selectList AttackableEnemy
              dmg <- getModifiedAttack a
              pushAll
                [ Ask ident $ ChooseOne $ map
                  (damageChoice a (toDamage dmg $ FromPlayerAttack ident))
                  enemies
                , CheckWindows [W.Window W.After $ W.MadeBasicAttack ident]
                ]
              pure a
            else do
              push $ IdentityMessage (heroIdentityId a) IdentityRemoveStunned
              pure a
        Thwarted -> do
          confused <- identityMatches ConfusedIdentity (heroIdentityId a)
          if not confused
            then do
              schemes <- selectList ThwartableScheme
              thw <- getModifiedThwart a
              push $ Ask ident $ ChooseOne $ map (thwartChoice a thw) schemes
              pure a
            else do
              push $ IdentityMessage (heroIdentityId a) IdentityRemoveConfused
              pure a
        Defended enemyId -> do
          def <- getModifiedDefense a
          pushAll
            [ IdentityMessage ident ExhaustedIdentity
            , IdentityMessage ident $ IdentityDefended def
            , case enemyId of
              EnemyVillainId vid ->
                VillainMessage vid (VillainDefendedBy $ IdentityCharacter ident)
              EnemyMinionId vid ->
                MinionMessage vid (MinionDefendedBy $ IdentityCharacter ident)
            ]
          pure a
        _ -> pure a
    _ -> pure a
