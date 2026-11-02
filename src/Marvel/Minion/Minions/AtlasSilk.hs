module Marvel.Minion.Minions.AtlasSilk where
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Phase

atlasSilk :: MinionCard AtlasSilk
atlasSilk = minion (AtlasSilk . (`With` (0 :: Natural))) Cards.atlasSilk (Sch 0) (Atk 3) (HP 18)
newtype AtlasSilk = AtlasSilk (Attrs Minion `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance IsMinion AtlasSilk where
  toMinionAttrs (AtlasSilk (attrs `With` _)) = attrs
  minionDisplayCounters (AtlasSilk (_ `With` n)) = n
  overMinionAttrs f (AtlasSilk (attrs `With` n)) = AtlasSilk (f attrs `With` n)
instance HasAbilities AtlasSilk where
  getAbilities u = [limitedWindowAbility u 1 (W.PhaseEnds W.After VillainPhase) ForcedResponse NoCriteria NoCost (runAbility u 1)]
instance HasModifiersFor AtlasSilk where
  getModifiersFor _ (MinionRef mid) (AtlasSilk (attrs `With` n)) | mid == minionId attrs = pure [HitPointModifier $ 2*n]
  getModifiersFor _ _ _ = pure []
instance RunMessage AtlasSilk where
  runMessage msg _u@(AtlasSilk (attrs `With` n)) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> pure $ AtlasSilk (attrs `With` (n+1))
    PlaceCounters target amount | isTarget attrs target -> pure $ AtlasSilk (attrs `With` (n+amount))
    RemoveCounters target amount | isTarget attrs target -> pure $ AtlasSilk (attrs `With` subtractNatural amount n)
    _ -> AtlasSilk . (`With` n) <$> runMessage msg attrs
