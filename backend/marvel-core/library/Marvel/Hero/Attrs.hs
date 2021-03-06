module Marvel.Hero.Attrs (
  module Marvel.Hero.Attrs,
  module X,
) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp as X
import Marvel.Id as X
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

hero ::
  (HeroAttrs -> a) ->
  CardDef ->
  HP GameValue ->
  HandSize ->
  Thw ->
  Atk ->
  Def ->
  CardBuilder IdentityId a
hero f cardDef hp hSize thw atk def =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \ident ->
        f $
          HeroAttrs
            { heroIdentityId = ident
            , heroBaseHandSize = hSize
            , heroBaseThwart = thw
            , heroBaseAttack = atk
            , heroBaseDefense = def
            , heroAlterEgoForms = [A]
            , heroStartingHP = hp
            , heroCardDef = cardDef
            }
    }

class IsHero a

type HeroCard a = CardBuilder IdentityId a

data HeroAttrs = HeroAttrs
  { heroIdentityId :: IdentityId
  , heroBaseHandSize :: HandSize
  , heroBaseThwart :: Thw
  , heroBaseAttack :: Atk
  , heroBaseDefense :: Def
  , heroAlterEgoForms :: [Side]
  , heroStartingHP :: HP GameValue
  , heroCardDef :: CardDef
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasStartingHP HeroAttrs where
  startingHP = heroStartingHP

instance HasHandSize HeroAttrs where
  handSize = heroBaseHandSize

instance HasCardCode HeroAttrs where
  toCardCode = toCardCode . heroCardDef

instance HasCardDef HeroAttrs where
  getCardDef = heroCardDef

instance IsSource HeroAttrs where
  toSource = IdentitySource . heroIdentityId

instance IsTarget HeroAttrs where
  toTarget = IdentityTarget . heroIdentityId

instance Entity HeroAttrs where
  type EntityId HeroAttrs = IdentityId
  type EntityAttrs HeroAttrs = HeroAttrs
  toId = heroIdentityId
  toAttrs = id

getModifiedAttack :: MonadGame env m => HeroAttrs -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ heroBaseAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedThwart :: MonadGame env m => HeroAttrs -> m Natural
getModifiedThwart attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unThw $ heroBaseThwart attrs) modifiers
 where
  applyModifier (ThwartModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedDefense :: MonadGame env m => HeroAttrs -> m Natural
getModifiedDefense attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unDef $ heroBaseDefense attrs) modifiers
 where
  applyModifier (DefenseModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

damageChoice :: HeroAttrs -> Damage -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid ->
    TargetLabel
      (VillainTarget vid)
      [ DamageEnemy (VillainTarget vid) (toSource attrs) dmg
      , Run
          [ CheckWindows
              [ W.Window W.After $
                  W.IdentityAttack (toId attrs) (EnemyVillainId vid)
              ]
          ]
      ]
  EnemyMinionId mid ->
    TargetLabel
      (MinionTarget mid)
      [ DamageEnemy (MinionTarget mid) (toSource attrs) dmg
      , Run
          [ CheckWindows
              [ W.Window W.After $
                  W.IdentityAttack (toId attrs) (EnemyMinionId mid)
              ]
          ]
      ]

thwartChoice :: HeroAttrs -> Natural -> SchemeId -> Choice
thwartChoice attrs thw = \case
  SchemeMainSchemeId vid ->
    TargetLabel
      (MainSchemeTarget vid)
      [ThwartScheme (MainSchemeTarget vid) (toSource attrs) thw]
  SchemeSideSchemeId sid ->
    TargetLabel
      (SideSchemeTarget sid)
      [ThwartScheme (SideSchemeTarget sid) (toSource attrs) thw]

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
                [ Ask ident $ ChooseOne $ map (damageChoice a (toDamage dmg $ FromPlayerAttack ident)) enemies
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
            , IdentityMessage ident $
                IdentityDefended def
            , case enemyId of
                EnemyVillainId vid ->
                  VillainMessage vid (VillainDefendedBy $ IdentityCharacter ident)
                EnemyMinionId vid ->
                  MinionMessage vid (MinionDefendedBy $ IdentityCharacter ident)
            ]
          pure a
        _ -> pure a
    _ -> pure a
