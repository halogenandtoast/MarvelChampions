module Marvel.Minion.Minions.Chimera where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Window qualified as W
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

chimera :: MinionCard Chimera
chimera = minion (Chimera . (`With` (0 :: Natural))) Cards.chimera (Sch 1) (Atk 1) (HP 5)

newtype Chimera = Chimera (Attrs Minion `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsMinion Chimera where
  toMinionAttrs (Chimera (attrs `With` _)) = attrs
  overMinionAttrs f (Chimera (attrs `With` n)) = Chimera (f attrs `With` n)
instance IsRef Chimera where
  toRef = toRef . toMinionAttrs

-- Wild is its own printed type here; a resource generator contributes only the
-- symbols printed on the face showing, not the resources its ability can generate.
printedResourcesControlled :: HasGame m => IdentityId -> Resource -> m Natural
printedResourcesControlled you resource = do
  allies <- selectList (AllyControlledBy $ IdentityWithId you)
  supports <- selectList (SupportControlledBy $ IdentityWithId you)
  upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
  schemes <- getControlledPlayerSideSchemes you False
  defs <- traverse (\target -> getsGame (`abilityCardDef` target))
    (map AllyRef allies <> map SupportRef supports <> map UpgradeRef upgrades <> map SideSchemeRef schemes)
  pure $ fromIntegral $ length $ filter (== resource) $ concatMap printedResources (catMaybes defs)

instance HasAbilities Chimera where
  getAbilities a@(Chimera (attrs `With` _)) =
    [windowAbility a 1 (W.EnemyActivates W.When (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity)
      ForcedInterrupt NoCost (runAbility a 1)]

instance HasModifiersFor Chimera where
  getModifiersFor _ target (Chimera (attrs `With` bonus)) = pure
    [modifier bonus | target == toRef attrs, modifier <- [AttackModifier, SchemeModifier]]

instance RunMessage Chimera where
  runMessage msg (Chimera (attrs `With` bonus)) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- printedResourcesControlled you Mental
      pure $ Chimera (attrs `With` n)
    CheckWindows windows | any ended windows -> do
      attrs' <- runMessage msg attrs
      pure $ Chimera (attrs' `With` 0)
    _ -> Chimera . (`With` bonus) <$> runMessage msg attrs
   where
    ended (W.Window W.After (W.EnemyActivation enemy _)) = enemy == EnemyMinionId (minionId attrs)
    ended _ = False
