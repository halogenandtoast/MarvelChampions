module Marvel.Treachery.Treacheries.DeathFromAbove (
  deathFromAbove,
  DeathFromAbove (..),
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
import Marvel.Game.Source
import Marvel.Id
import Marvel.Modifier

deathFromAbove :: TreacheryCard DeathFromAbove
deathFromAbove = treachery (DeathFromAbove . (`With` Meta Nothing)) Cards.deathFromAbove

-- | The villain and its extra SCH or ATK, while it schemes or attacks.
newtype Meta = Meta {bonus :: Maybe (VillainId, Modifier)}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DeathFromAbove = DeathFromAbove (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery DeathFromAbove where
  toTreacheryAttrs (DeathFromAbove (attrs `With` _)) = attrs
  treacheryModifiersFor _ (VillainRef vid) (DeathFromAbove (_ `With` meta)) =
    pure [m | Just (vid', m) <- [bonus meta], vid' == vid]
  treacheryModifiersFor _ _ _ = pure []

-- | When Revealed (Alter-Ego): Green Goblin schemes with +X SCH; (Hero): he attacks
-- with +X ATK, X the villain's stage.
instance RunMessage DeathFromAbove where
  runMessage msg (DeathFromAbove (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      selectOne ActiveVillain >>= \case
        Nothing -> pure $ DeathFromAbove (attrs `With` meta)
        Just villain -> do
          stage <- getVillainStage villain
          hero <- identityMatches HeroIdentity you
          if hero then enemyAttacks villain you else villainSchemes villain
          let m = if hero then AttackModifier stage else SchemeModifier stage
          pure $ DeathFromAbove (attrs `With` Meta (Just (villain, m)))
    _ -> DeathFromAbove . (`With` meta) <$> runMessage msg attrs
