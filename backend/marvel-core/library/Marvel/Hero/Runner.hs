{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Hero.Runner
  ( module X
  ) where

import Marvel.Prelude

import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Damage
import Marvel.Hero.Types as X
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Window qualified as W

instance HasAbilities Hero where
  getAbilities (Hero a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability a 300 Basic (SchemeExists ThwartableScheme) ExhaustCost Thwart
      , ability a 301 Basic (EnemyExists AttackableEnemy) ExhaustCost Attack
      ]

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
