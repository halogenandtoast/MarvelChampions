module Marvel.Treachery.Treacheries.Overconfidence (
  overconfidence,
  Overconfidence (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Damage
import Marvel.Id
import Marvel.Window qualified as W

overconfidence :: TreacheryCard Overconfidence
overconfidence = treachery (Overconfidence . (`With` Meta Nothing 0)) Cards.overconfidence

-- | The villain activating, and the threat or damage its activation has placed.
data Meta = Meta {activating :: Maybe VillainId, placed :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Overconfidence = Overconfidence (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery Overconfidence where
  toTreacheryAttrs (Overconfidence (attrs `With` _)) = attrs

-- | When Revealed (Alter-Ego): Green Goblin schemes; (Hero): he attacks you. If at least
-- 3 threat (damage) was placed by the activation, this card gains surge.
-- The damage counted is that taken, after the windows for it: none a tough status card
-- prevented.
instance RunMessage Overconfidence where
  runMessage msg (Overconfidence (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident msg' | ident == treacheryId attrs -> case msg' of
      RevealTreachery you -> do
        mvillain <- selectOne ActiveVillain
        hero <- identityMatches HeroIdentity you
        for_ mvillain \villain -> if hero then enemyAttacks villain you else villainSchemes villain
        pure $ Overconfidence (attrs `With` meta {activating = mvillain})
      CheckTreacheryCondition _ ->
        pure . Overconfidence $ (if placed meta >= 3 then attrs & surgeL .~ True else attrs) `With` meta
      _ -> Overconfidence . (`With` meta) <$> runMessage msg attrs
    MainSchemeMessage _ (MainSchemePlaceThreat n) | isJust (activating meta) -> counted n
    CheckWindows ws -> counted $ sum [damageAmount d | W.Window W.After (taken -> Just (src, d)) <- ws, byVillain src d]
    _ -> Overconfidence . (`With` meta) <$> runMessage msg attrs
   where
    counted n = pure $ Overconfidence (attrs `With` meta {placed = placed meta + n})
    taken = \case
      W.IdentityTookDamage _ src d -> Just (src, d)
      W.AllyTookDamage _ src d -> Just (src, d)
      _ -> Nothing
    byVillain src d = damageSource d == FromAttack && maybe False ((== src) . VillainRef) (activating meta)
