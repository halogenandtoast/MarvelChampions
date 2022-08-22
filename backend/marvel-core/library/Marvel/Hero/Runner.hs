module Marvel.Hero.Runner
  ( module Marvel.Hero.Runner
  , module X
  ) where

import Marvel.Hero.Types as X

instance HasAbilities Hero where
  getAbilities (Hero a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability
        a
        300
        Basic
        (SchemeExists ThwartableScheme)
        ExhaustCost
        Ability.Thwart
      , ability
        a
        301
        Basic
        (EnemyExists AttackableEnemy)
        ExhaustCost
        Ability.Attack
      ]

instance RunMessage HeroAttrs where
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
