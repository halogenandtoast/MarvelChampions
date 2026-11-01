module Marvel.Ally.Allies.MutantGenesis where

import Marvel.Id
import Marvel.Prelude
import Marvel.Entity
import Marvel.Card
import Marvel.GameValue ()
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ally.Runner
import Marvel.Ally.Types ()
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Modifier ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Keyword
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Campaign (CampaignRecord (RecordMutantCaptives))
import Marvel.Ally.Cards qualified as Cards
import Marvel.Stats ()
import Marvel.Hp ()

newtype MutantGenesisAlly = MutantGenesisAlly (Attrs Ally `With` Bool)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAlly MutantGenesisAlly where
  toAllyAttrs (MutantGenesisAlly (a `With` _)) = a
  allyHasThwart (MutantGenesisAlly (a `With` _)) = toCardCode a /= "32066"
  allyHasAttack (MutantGenesisAlly (a `With` _)) = toCardCode a /= "32066"

mkMutantGenesisAlly :: Attrs Ally -> MutantGenesisAlly
mkMutantGenesisAlly a = MutantGenesisAlly (a `With` False)

mgRobertKelly :: AllyCard MutantGenesisAlly
mgRobertKelly = ally mkMutantGenesisAlly Cards.mgRobertKelly (Thw 0, 1) (Atk 0, 1) (HP 9)

mgJubilee :: AllyCard MutantGenesisAlly
mgJubilee = ally mkMutantGenesisAlly Cards.mgJubilee (Thw 1, 1) (Atk 1, 1) (HP 3)

mgRictor :: AllyCard MutantGenesisAlly
mgRictor = ally mkMutantGenesisAlly Cards.mgRictor (Thw 1, 1) (Atk 1, 1) (HP 3)

mgBoomBoom :: AllyCard MutantGenesisAlly
mgBoomBoom = ally mkMutantGenesisAlly Cards.mgBoomBoom (Thw 1, 1) (Atk 1, 1) (HP 3)

mgCannonball :: AllyCard MutantGenesisAlly
mgCannonball = ally mkMutantGenesisAlly Cards.mgCannonball (Thw 1, 1) (Atk 2, 1) (HP 3)

mgWolfsbane :: AllyCard MutantGenesisAlly
mgWolfsbane = ally mkMutantGenesisAlly Cards.mgWolfsbane (Thw 1, 1) (Atk 2, 1) (HP 3)

instance HasModifiersFor MutantGenesisAlly where
  getModifiersFor _ target (MutantGenesisAlly (a `With` defeated))
    | target == toRef a = pure $ case toCardCode a of
        "32066" -> [ControlledByFirstPlayer, NotCountedAgainstAllyLimit, CannotHaveAttachedPlayerCards, CannotAttack]
        "32088b" -> [ControlledByFirstPlayer, NotCountedAgainstAllyLimit]
        "32091" -> [ConsequentialDamageReduction 1 | defeated]
        "32092" -> [KeywordModifier Piercing]
        _ -> []
  getModifiersFor _ _ _ = pure []

instance HasAbilities MutantGenesisAlly where
  getAbilities (MutantGenesisAlly (a `With` _)) =
    [ability a 1 Action OwnsThis (ExhaustCost <> ResourceCost (Just Energy)) (runAbility a 1) | code == "32088b"]
    <> [windowAbility a 2 (W.AllyAttacked W.After (AllyWithId $ allyId a) AnyEnemy) Response NoCost (runAbility a 2) | code `elem` ["32089", "32090"]]
   where code = toCardCode a

instance RunMessage MutantGenesisAlly where
  runMessage msg e@(MutantGenesisAlly (attrs `With` defeated)) = runQueueT $ case msg of
    CheckWindows windows | Trait.Captive `member` cdTraits (getCardDef attrs), any (\case W.Window W.After (W.AllyEnteredPlay aid) -> aid == allyId attrs; _ -> False) windows -> do
      recordInCampaignLog (RecordMutantCaptives [toCardCode attrs])
      pure e
    CheckWindows windows | toCardCode attrs == "32091", any (\case W.Window W.After (W.DefeatedMinion _ _ source _) -> source == toSource attrs; _ -> False) windows -> pure $ MutantGenesisAlly (attrs `With` True)
    AllyMessage aid AllyAttacked | aid == allyId attrs -> do
      a <- runMessage msg attrs
      pure $ MutantGenesisAlly (a `With` False)
    AllyMessage aid AllyThwarted | aid == allyId attrs -> do
      a <- runMessage msg attrs
      pure $ MutantGenesisAlly (a `With` False)
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList AnyEnemy
      unless (null enemies) $ chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage 2 FromAbility) | enemy <- enemies]
      pure e
    RanAbility you (isTarget attrs -> True) 2 windows _ -> do
      if toCardCode attrs == "32089"
        then withTheVillainIfAny you $ \villain -> do
          minions <- selectList (MinionEngagedWith $ IdentityWithId you)
          dealDamageToEach (maybe [] (pure . EnemyVillainId) villain <> map EnemyMinionId minions) attrs (toDamage 1 FromAbility)
        else for_ (concatMap (\case W.AllyAttack _ enemy -> [enemy]; W.AllyAttackCompletedAgainst _ enemies -> enemies; _ -> []) windows) (\enemy -> do
          present <- selectAny (EnemyWithId enemy)
          when present $ cardSpecific GameRef "mgAddBombCounter" enemy)
      pure e
    _ -> MutantGenesisAlly . (`With` defeated) <$> runMessage msg attrs
