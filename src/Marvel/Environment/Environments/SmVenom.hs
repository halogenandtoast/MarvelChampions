module Marvel.Environment.Environments.SmVenom where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window hiding (VillainDamaged)
import Marvel.Window qualified as W

newtype BellTowerMeta = BellTowerMeta {venom :: Maybe VillainId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SmVenomEnvironment = SmVenomEnvironment (Attrs Environment `With` BellTowerMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEnvironment SmVenomEnvironment where
  toEnvironmentAttrs (SmVenomEnvironment (a `With` _)) = a

bellTower :: Attrs Environment -> SmVenomEnvironment
bellTower = SmVenomEnvironment . (`With` BellTowerMeta Nothing)

smBellTowerA, smBellTowerB :: EnvironmentCard SmVenomEnvironment
smBellTowerA = environment bellTower Cards.smBellTowerA
smBellTowerB = environment bellTower Cards.smBellTowerB

quiet :: Attrs Environment -> Bool
quiet = (== "27077a") . toCardCode

instance HasAbilities SmVenomEnvironment where
  getAbilities (SmVenomEnvironment (a `With` meta)) = case venom meta of
    Nothing -> []
    Just vid
      | quiet a -> [windowAbility a 1 (VillainWouldTakeDamage (VillainWithId vid) DamageFromAnAttack) Interrupt NoCost (runAbility a 1)]
      | environmentCounters a > 0 ->
          [windowAbility a 2 (IdentityWouldTakeAttackDamageFrom (EnemyWithId $ EnemyVillainId vid) AnyIdentity) ForcedInterrupt NoCost (runAbility a 2)]
      | otherwise -> []

instance HasModifiersFor SmVenomEnvironment where
  getModifiersFor _ (VillainRef vid) (SmVenomEnvironment (a `With` meta))
    | venom meta == Just vid, not (quiet a) = do
        blanked <- elem Blank <$> getModifiers a
        pure [DamageTakenModifier 1 | not blanked]
  getModifiersFor _ _ _ = pure []

bellTowers :: (HasGame m) => m [EnvironmentId]
bellTowers = (<>) <$> selectList (EnvironmentIs Cards.smBellTowerA) <*> selectList (EnvironmentIs Cards.smBellTowerB)

removeChimes :: (ReverseQueue m) => Natural -> m ()
removeChimes n = bellTowers >>= traverse_ (`removeCounters` n)

instance RunMessage SmVenomEnvironment where
  runMessage msg e@(SmVenomEnvironment (attrs `With` meta)) = runQueueT $ case msg of
    EnvironmentMessage eid EnvironmentEntered | eid == environmentId attrs -> do
      vid <- selectOne ActiveVillain
      pure $ SmVenomEnvironment (attrs `With` BellTowerMeta vid)
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(vid, damageAmount damage) | W.DamagedVillain vid damage <- windows, damageAmount damage > 0] \(vid, n) ->
        insteadOf (\case VillainMessage other (VillainDamaged _ _) -> other == vid; _ -> False) (placeCounters attrs n)
      pure e
    RanAbility _ (isTarget attrs -> True) 2 windows _ -> do
      for_ [(you, damage) | W.IdentityTakeDamageFrom you _ damage <- windows] \(you, damage) -> do
        let prevented = min (environmentCounters attrs) (damageAmount damage)
        when (prevented > 0) do
          preventDamage you prevented
          removeCounters attrs prevented
      pure e
    CardSpecific (isTarget attrs -> True) "resetQuiet" _ -> do
      unless (quiet attrs) $ flipEnvironment (environmentId attrs)
      pure $ SmVenomEnvironment (attrs {environmentCounters = 0} `With` meta)
    PlaceCounters target _ | target == toRef attrs -> changed
    RemoveCounters target _ | target == toRef attrs -> changed
    RemoveFromPlay (SideSchemeRef _) -> e <$ cardSpecific attrs "checkSide" ()
    DefeatedLeavesPlay (SideSchemeRef _) -> e <$ cardSpecific attrs "checkSide" ()
    CardSpecific (isTarget attrs -> True) "checkSide" _ -> checkSide attrs
    CheckWindows _ -> checkSide attrs
    _ -> SmVenomEnvironment . (`With` meta) <$> runMessage msg attrs
   where
    changed = do
      updated <- runMessage msg attrs
      checkSide updated
    checkSide current = do
      blanked <- elem Blank <$> getModifiers current
      threshold <- fromIntegral <$> fromGameValue (PerPlayer 3)
      unless blanked $
        when ((quiet current && environmentCounters current >= threshold) || (not (quiet current) && environmentCounters current == 0)) $
          flipEnvironment (environmentId current)
      pure $ SmVenomEnvironment (current `With` meta)
