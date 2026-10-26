module Marvel.Treachery.Treacheries.BeetleMania (
  beetleMania,
  BeetleMania (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

beetleMania :: TreacheryCard BeetleMania
beetleMania = treachery (BeetleMania . (`With` Meta Nothing False)) Cards.beetleMania

-- | Beetle, given +1 ATK while this card resolves, and whether he attacked.
data Meta = Meta {attacker :: Maybe MinionId, attacked :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BeetleMania = BeetleMania (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery BeetleMania where
  toTreacheryAttrs (BeetleMania (attrs `With` _)) = attrs
  treacheryModifiersFor _ (MinionRef mid) (BeetleMania (_ `With` meta)) =
    pure [AttackModifier 1 | attacker meta == Just mid]
  treacheryModifiersFor _ _ _ = pure []

-- | When Revealed (Alter-Ego): this card gains surge. (Hero): Beetle attacks you with +1
-- ATK. If no attack was made this way (Beetle not in play, or stunned), this card gains
-- surge.
instance RunMessage BeetleMania where
  runMessage msg (BeetleMania (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      mBeetle <- if isHero then selectOne (MinionIs Minions.beetle) else pure Nothing
      case mBeetle of
        Nothing -> pure . BeetleMania $ (attrs & surgeL .~ True) `With` meta
        Just beetle -> do
          enemyAttacks beetle you
          cardSpecific attrs "attacked" ()
          pure . BeetleMania $ attrs `With` meta {attacker = Just beetle}
    MinionMessage mid MinionAttacked | attacker meta == Just mid ->
      pure . BeetleMania $ attrs `With` meta {attacked = True}
    CardSpecific (isTarget attrs -> True) "attacked" _ ->
      pure . BeetleMania $ (attrs & surgeL .~ not (attacked meta)) `With` meta {attacker = Nothing}
    _ -> BeetleMania . (`With` meta) <$> runMessage msg attrs
