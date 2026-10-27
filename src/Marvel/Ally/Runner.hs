{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Ally.Runner
  ( module X
  ) where

import Marvel.Prelude

import Marvel.Ally.Types as X hiding (Field(..))
import Marvel.Message.Lifted as X
import Marvel.Question as X

import Data.HashSet qualified as HashSet
import Marvel.Game.Source (getModifiers)
import Marvel.Ability
import Marvel.Attack
import Marvel.Cost
import Marvel.Criteria
import Marvel.Card.Def
import Marvel.Damage
import Marvel.Keyword
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Query
import Marvel.Window qualified as W

instance HasAbilities Ally where
  getAbilities x@(Ally a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability
          x
          300
          Basic
          -- "you cannot thwart" (Baron Zemo) keeps the identity from thwarting, not
          -- its allies
          (SchemeExists SchemeYouCanThwart)
          ExhaustCost
          (AllyThwart $ toId x)
      | unThw (allyThwart $ toAllyAttrs a) > 0
      ]
      <> [ ability
             x
             301
             Basic
             -- a stunned ally can attempt an attack with no target ("Stunned")
             (if allyStunned (toAllyAttrs a) then NoCriteria else EnemyExists $ AttackableByAlly $ toId x)
             (ExhaustCost <> allyAdditionalAttackCost (toAllyAttrs a))
             (AllyAttack $ toId x)
         ]

statusL :: StatusCard -> Lens' (Attrs Ally) Bool
statusL = \case
  StunnedStatus -> stunnedL
  ConfusedStatus -> confusedL
  ToughStatus -> toughL

instance RunMessage (Attrs Ally) where
  runMessage msg a = runQueueT $ case msg of
    AllyMessage ident msg' | ident == allyId a -> case msg' of
      ReadiedAlly -> do
        pure $ a & exhaustedL .~ False
      ExhaustedAlly -> do
        pure $ a & exhaustedL .~ True
      AllyStunned -> do
        pure $ a & stunnedL .~ True
      AllyAttacked -> if allyStunned a
        then pure $ a & stunnedL .~ False
        else do
          enemies <- selectList $ AttackableByAlly $ allyId a
          -- "[star] its basic attack gains piercing (ranged)": the keyword on the ally, or
          -- one it gains ("its attacks gain overkill")
          modifiers <- getModifiers a
          let keywords = toList (cdKeywords $ allyCardDef a) <> [k | KeywordModifier k <- modifiers]
              gains =
                [withPiercing | Piercing `elem` keywords]
                  <> [withRanged | Ranged `elem` keywords]
                  <> [withOverkill | Overkill `elem` keywords]
          dmg <- foldr ($) <$> (toDamage <$> getModifiedAttack a <*> pure (FromAllyAttack $ allyId a)) <*> pure gains
          chooseOne (allyController a)
            [ targeting (enemyRef enemy) do
                checkWindows [W.Window W.When $ W.AllyAttack (allyId a) enemy]
                allyAttackLands (allyId a) enemy dmg
            | enemy <- enemies
            ]
          pure a
      -- an interrupt that defeats the enemy aborts the attack: no damage and no
      -- consequential damage (RR, Consequential Damage)
      AllyAttackLands enemy dmg -> do
        stillInPlay <- selectAny (EnemyWithId enemy)
        consequential <- getModifiedAttackConsequentialDamage a
        when stillInPlay do
          dealDamage enemy a dmg
          unless (damageRanged dmg) $ enemyRetaliates enemy (AllyCharacter $ allyId a)
          checkWindows [W.Window W.After $ W.AllyAttack (allyId a) enemy]
          when (consequential > 0) $ damageCharacter ident a (toDamage consequential FromConsequential)
        pure a
      AllyThwarted -> if allyConfused a
        then pure $ a & confusedL .~ False
        else do
          schemes <- selectList SchemeYouCanThwart
          thw <- getModifiedThwart a
          atk <- getModifiedAttack a
          -- a scheme thwarted with ATK instead of THW, as The Red House allows
          choices <- for schemes \scheme -> do
            withAttack <- elem ThwartWithAttack <$> getModifiers (schemeRef scheme)
            let thwartWith n = do
                  checkWindows [W.Window W.When $ W.AllyThwart (allyId a) scheme]
                  basicThwartOf scheme a n
                  checkWindows [W.Window W.After $ W.AllyThwart (allyId a) scheme]
            pure $ targeting (schemeRef scheme) $
              if withAttack && atk /= thw
                then
                  chooseOne (allyController a)
                    [labeled "$label.thwartWithThw" (thwartWith thw), labeled "$label.thwartWithAtk" (thwartWith atk)]
                else thwartWith thw
          chooseOne (allyController a) choices
          allyStep ident AllyThwartConsequential
          pure a
      AllyThwartConsequential -> do
        consequential <- getModifiedThwartConsequentialDamage a
        when (consequential > 0) $ damageCharacter ident a (toDamage consequential FromConsequential)
        pure a
      AllyDefended enemyId -> do
        exhaust ident
        enemyDefendedBy enemyId (AllyCharacter ident)
        pure a
      AllyHealed n -> pure $ a & damageL %~ subtractNatural n
      AllyWasAttacked attack' -> do
        hitPoints <- getModifiedHitPoints a
        let
          overkill = subtractNatural
            (subtractNatural (allyDamage a) hitPoints)
            (attackDamageDealt attack')
        -- an attack dealing indirect damage (Starshark) has it divided among the
        -- characters the ally's controller controls, overkill having none past it
        when (attackDamageDealt attack' > 0) $
          if attackIndirect attack'
            then dealIndirectAttackDamage (allyController a) (attackSource attack') (attackDamageTo attack' (attackDamageDealt attack'))
            else damageCharacter ident (attackSource attack') (attackDamageTo attack' (attackDamageDealt attack'))
        -- the excess is the attack's damage to the controller: "when you would take
        -- damage" answers it, as it does the attack's own
        when (attackOverkill attack' && overkill > 0 && not (attackIndirect attack')) do
          let excess = toDamage overkill FromAttack
          checkWindows [W.Window W.When $ W.IdentityTakeDamage (allyController a) excess]
          attackDamageIdentity (allyController a) (attackSource attack') excess
        -- retaliate, if it is still in play after the attack, and the attack not ranged
        let retaliates = sum [n | Retaliate n <- toList (cdKeywords $ allyCardDef a)]
        when (retaliates > 0 && not (attackRanged attack')) $
          allyStep ident (AllyRetaliates (attackEnemy attack') retaliates)
        -- "after <ally> defends against an attack": the attack has resolved
        when (attackDefended attack') $
          checkWindows [W.Window W.After $ W.AllyDefendedAgainst ident (attackEnemy attack')]
        pure a
      AllyRetaliates enemy n -> do
        dealDamage enemy a (toDamage n FromRetaliate)
        pure a
      AllyDamaged source damage -> if allyTough a && preventedByTough damage
        then pure $ a & toughL .~ False
        else do
          hitPoints <- getModifiedHitPoints a
          attackDealtDamageTo source damage ident
          when (damageAmount damage + allyDamage a >= hitPoints) $
            case sourceEnemy source of
              Just enemy | damageSource damage == FromAttack -> allyStep ident (AllyDefeatedByAttack enemy)
              _ -> defeatAlly ident
          when (damageAmount damage > 0) $ checkWindows [W.Window W.After $ W.AllyTookDamage ident source damage]
          pure $ a & damageL +~ damageAmount damage & toughL %~ (&& not (piercesTough damage))
      AllyDefeated -> defeated Nothing
      AllyDefeatedByAttack enemy -> defeated (Just enemy)
      SpendAllyUse -> pure $ a & countersL -~ 1
      AllyPaidWith payment -> pure a {allyPaidWith = payment}
      AllyOwnedBy owner -> pure a {allyOwner = owner}
      AttachedUpgradeToAlly upgradeId ->
        pure $ a & upgradesL %~ HashSet.insert upgradeId
    -- "if an ability that says an ally gets +X hit points ceases to be in effect and
    -- causes that ally to have damage on it equal to or greater than its hit points,
    -- that ally is defeated"
    UpgradeRemoved upgradeId | HashSet.member upgradeId (allyUpgrades a) -> do
      hitPoints <- getModifiedHitPoints a
      when (allyDamage a > 0 && allyDamage a >= hitPoints) $ defeatAlly (allyId a)
      pure $ a & upgradesL %~ HashSet.delete upgradeId
    -- counters on the ally, the uses its abilities spend (Jack Flag's ammo)
    PlaceCounters (AllyRef ident) n | ident == allyId a -> pure $ a & countersL +~ n
    RemoveCounters (AllyRef ident) n | ident == allyId a -> pure $ a & countersL %~ subtractNatural n
    GiveStatus (AllyRef ident) s | ident == allyId a -> pure $ a & statusL s .~ True
    RemoveStatus (AllyRef ident) s | ident == allyId a -> pure $ a & statusL s .~ False
    _ -> pure a
   where
    -- leaving play discards it
    defeated byAttack = do
      let card = MkPlayerCard (CardId $ unAllyId $ allyId a) (allyCardDef a) (Just $ allyOwner a) (Just $ allyController a)
      checkWindows [W.Window W.When $ W.DefeatedAlly (allyId a) byAttack]
      -- which discards it
      removeFromPlay a
      checkWindows [W.Window W.After $ W.AllyCardDefeated (allyController a) card]
      pure a
