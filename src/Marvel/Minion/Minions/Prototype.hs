module Marvel.Minion.Minions.Prototype where
import Marvel.Prelude
import Marvel.Card ()
import Marvel.Game.Source ()
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Domino ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Identity.Types
import Marvel.Matchers ()
import Marvel.Payment ()
import Marvel.Projection
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

prototype :: MinionCard Prototype
prototype = minion (Prototype . (`With` (0 :: Natural))) Cards.prototype (Sch 2) (Atk 2) (HP 1)
newtype Prototype = Prototype (Attrs Minion `With` Natural)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsMinion Prototype where
  toMinionAttrs (Prototype (attrs `With` _)) = attrs
  overMinionAttrs f (Prototype (attrs `With` meta)) = Prototype (f attrs `With` meta)
  minionDisplayCounters (Prototype (_ `With` counters)) = counters
instance IsRef Prototype where
  toRef = toRef . toMinionAttrs

instance HasModifiersFor Prototype where
  getModifiersFor _ (MinionRef mid) (Prototype (attrs `With` counters)) | mid == minionId attrs = pure [HitPointModifier $ counters]
  getModifiersFor _ _ _ = pure []

instance RunMessage Prototype where
  runMessage msg a@(Prototype (attrs `With` counters)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      n <- project PlayerIdentityDamage you
      placeCounters attrs n
      pure a
    PlaceCounters target n | target == toTarget attrs -> pure $ Prototype (attrs `With` (counters + n))
    RemoveCounters target n | target == toTarget attrs -> pure $ Prototype (attrs `With` subtractNatural n counters)
    _ -> Prototype . (`With` counters) <$> runMessage msg attrs
