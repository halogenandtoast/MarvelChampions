-- | The four roles' consumable campaign upgrades (MC32 176–195).
module Marvel.Upgrade.Upgrades.MutantGenesisCampaign where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as A
import Marvel.Campaign
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Effect.Types qualified as E
import Marvel.Entity
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question (Choice (Pay), runAbility)
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as T
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

newtype MutantGenesisRoleUpgrade = MutantGenesisRoleUpgrade (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mgCoupDeGrace, mgSwagger, mgBrazenDefense, mgFerociousAttack, mgWarCry,
  mgCommanderCoupDeGrace, mgCompassion, mgGroupAssault, mgShockAndAwe, mgImprovisation,
  mgDefenderSwagger, mgSurprise, mgHeroicIntervention, mgDeterminedDefense, mgBodyguard,
  mgPeacekeeperSurprise, mgPeacekeeperCompassion, mgRescueOperation, mgMentorship, mgFortitude :: UpgradeCard MutantGenesisRoleUpgrade
mgCoupDeGrace = upgrade MutantGenesisRoleUpgrade Cards.mgCoupDeGrace
mgSwagger = upgrade MutantGenesisRoleUpgrade Cards.mgSwagger
mgBrazenDefense = upgrade MutantGenesisRoleUpgrade Cards.mgBrazenDefense
mgFerociousAttack = upgrade MutantGenesisRoleUpgrade Cards.mgFerociousAttack
mgWarCry = upgrade MutantGenesisRoleUpgrade Cards.mgWarCry
mgCommanderCoupDeGrace = upgrade MutantGenesisRoleUpgrade Cards.mgCommanderCoupDeGrace
mgCompassion = upgrade MutantGenesisRoleUpgrade Cards.mgCompassion
mgGroupAssault = upgrade MutantGenesisRoleUpgrade Cards.mgGroupAssault
mgShockAndAwe = upgrade MutantGenesisRoleUpgrade Cards.mgShockAndAwe
mgImprovisation = upgrade MutantGenesisRoleUpgrade Cards.mgImprovisation
mgDefenderSwagger = upgrade MutantGenesisRoleUpgrade Cards.mgDefenderSwagger
mgSurprise = upgrade MutantGenesisRoleUpgrade Cards.mgSurprise
mgHeroicIntervention = upgrade MutantGenesisRoleUpgrade Cards.mgHeroicIntervention
mgDeterminedDefense = upgrade MutantGenesisRoleUpgrade Cards.mgDeterminedDefense
mgBodyguard = upgrade MutantGenesisRoleUpgrade Cards.mgBodyguard
mgPeacekeeperSurprise = upgrade MutantGenesisRoleUpgrade Cards.mgPeacekeeperSurprise
mgPeacekeeperCompassion = upgrade MutantGenesisRoleUpgrade Cards.mgPeacekeeperCompassion
mgRescueOperation = upgrade MutantGenesisRoleUpgrade Cards.mgRescueOperation
mgMentorship = upgrade MutantGenesisRoleUpgrade Cards.mgMentorship
mgFortitude = upgrade MutantGenesisRoleUpgrade Cards.mgFortitude

instance HasAbilities MutantGenesisRoleUpgrade where
  getAbilities a@(MutantGenesisRoleUpgrade attrs) = map withSubtype $ case toCardCode attrs of
    "32176" -> coup
    "32181" -> coup
    "32177" -> swagger
    "32186" -> swagger
    "32178" -> [window]
    "32179" -> [action HeroAction (ResourceCost Nothing <> ResourceCost Nothing <> ResourceCost Nothing)]
    "32180" -> resource [T.Attack,T.Defense]
    "32182" -> compassion
    "32192" -> compassion
    "32183" -> [action HeroAction NoCost]
    "32184" -> [action HeroAction (MultiResourceCost [Nothing,Nothing,Nothing])]
    "32185" -> resource [T.Attack,T.Tactic]
    "32187" -> surprise
    "32191" -> surprise
    "32188" -> [action HeroAction (MultiResourceCost [Nothing,Nothing,Nothing])]
    "32189" -> [limitedWindowAbility a 1 (W.HeroDefended W.When You AnyEnemy) HeroInterrupt OwnsThis (MultiResourceCost [Nothing,Nothing]) (runAbility a 1)]
    "32190" -> resource [T.Defense,T.Thwart]
    "32193" -> [action HeroAction NoCost]
    "32194" -> [action HeroAction (MultiResourceCost [Nothing,Nothing,Nothing])]
    "32195" -> resource [T.Tactic,T.Thwart]
    _ -> []
   where
    withSubtype ab = ab {abilitySubType = subtype}
    subtype = case toCardCode attrs of
      "32177" -> Just A.Defense
      "32186" -> Just A.Defense
      "32178" -> Just A.Defense
      "32189" -> Just A.Defense
      "32179" -> Just A.Attack
      "32184" -> Just A.Attack
      "32188" -> Just A.Thwart
      "32194" -> Just A.Thwart
      _ -> Nothing
    action typ cost = ability a 1 typ OwnsThis cost (runAbility a 1)
    coup = [limitedWindowAbility a 1 (W.IdentityAttacked W.When You AnyEnemy) HeroInterrupt OwnsThis NoCost (runAbility a 1)]
    swagger = [limitedWindowAbility a 1 (W.MakesBasicDefense W.When You AnyEnemy) HeroInterrupt OwnsThis NoCost (runAbility a 1)]
    compassion = [limitedWindowAbility a 1 (W.UsesRecovery You) AlterEgoResponse OwnsThis NoCost (runAbility a 1)]
    surprise = [limitedWindowAbility a 1 (W.HeroThwarted W.After You) HeroResponse OwnsThis NoCost (runAbility a 1)]
    resource traits = [(action HeroResource NoCost)
      {abilityCriteria = OwnsThis <> PayingFor (CardWithType EventType <> CardMatchesAny (map CardWithTrait traits))
      ,abilityChoices = [Pay (ResourcePayment Wild <> ResourcePayment Wild),runAbility a 1]}]
    window = limitedWindowAbility a 1 (W.EnemyAttacked W.When AnyEnemy AnyIdentity) HeroInterrupt OwnsThis (ResourceCost Nothing) (runAbility a 1)

instance RunMessage MutantGenesisRoleUpgrade where
  runMessage msg u@(MutantGenesisRoleUpgrade attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      case toCardCode attrs of
        "32176" -> modifyCurrentPlayerAttack you 3 [Overkill]
        "32181" -> modifyCurrentPlayerAttack you 3 [Overkill]
        "32177" -> addToBasicPower you 3 >> ready you
        "32186" -> addToBasicPower you 3 >> ready you
        "32178" -> for_ [enemy | W.EnemyAttack enemy _ <- windows] \enemy -> do
          preventEnemyAttackDamage enemy 3
          dealDamage enemy attrs (toDamage 3 $ FromPlayerAttack you)
        "32179" -> do
          chooseDamage you attrs (toDamage 6 $ FromPlayerAttack you) AttackableEnemy
          ready you
        "32180" -> giveStatus you ToughStatus
        "32182" -> healDivided you attrs 3 (CharacterControlledBy $ IdentityWithId you) >> drawCards you 1
        "32192" -> healDivided you attrs 3 (CharacterControlledBy $ IdentityWithId you) >> drawCards you 1
        "32183" -> createEffect (getCardDef attrs) attrs (AllyEntity AnyAlly)
        "32184" -> do
          chooseDamage you attrs (toDamage 6 $ FromPlayerAttack you) AttackableEnemy
          readyAllies you
        "32185" -> do
          allies <- selectList AnyAlly
          unless (null allies) $ chooseOne you [targeting ally (ready ally >> heal ally 2) | ally <- allies]
        "32187" -> removeThreatDivided you attrs 3 AnyScheme >> chooseConfuse you
        "32191" -> removeThreatDivided you attrs 3 AnyScheme >> chooseConfuse you
        "32188" -> thwartDivided you attrs 5 ThwartableScheme >> giveStatus you ToughStatus
        "32189" -> for_ [enemy | W.HeroDefends _ enemy <- windows] \enemy -> replaceAttackDamageWithThreatRemoval enemy you
        "32190" -> drawCards you 1
        "32193" -> createEffect (getCardDef attrs) attrs (AllyEntity AnyAlly)
        "32194" -> thwartDivided you attrs 5 ThwartableScheme >> readyAllies you
        "32195" -> chooseStun you
        _ -> pure ()
      removeFromGame attrs
      recordInCampaignLog (RemoveMutantRoleUpgrade $ toCardCode attrs)
      pure u
    _ -> MutantGenesisRoleUpgrade <$> runMessage msg attrs
   where
    readyAllies you = selectList (AllyControlledBy $ IdentityWithId you) >>= traverse_ ready
    chooseConfuse you = do
      enemies <- selectList EnemyCanBeConfused
      unless (null enemies) $ chooseOne you [targeting (enemyRef enemy) (confuse enemy attrs) | enemy <- enemies]
    chooseStun you = do
      enemies <- selectList EnemyCanBeStunned
      unless (null enemies) $ chooseOne you [targeting (enemyRef enemy) (stun enemy attrs) | enemy <- enemies]

newtype MutantGenesisRoleEffect = MutantGenesisRoleEffect (Attrs E.Effect)
  deriving anyclass E.IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

mgGroupAssaultEffect, mgRescueOperationEffect :: E.CardEffect MutantGenesisRoleEffect
mgGroupAssaultEffect = E.effectWith MutantGenesisRoleEffect Cards.mgGroupAssault
  ((E.modifiersL .~ [NoAttackConsequentialDamage]) . (E.endsL ?~ E.DisableAtEndOfPhase))
mgRescueOperationEffect = E.effectWith MutantGenesisRoleEffect Cards.mgRescueOperation
  ((E.modifiersL .~ [NoThwartConsequentialDamage]) . (E.endsL ?~ E.DisableAtEndOfPhase))

instance RunMessage MutantGenesisRoleEffect where
  runMessage msg (MutantGenesisRoleEffect attrs) = MutantGenesisRoleEffect <$> runMessage msg attrs
