module Marvel.Treachery.Treacheries.RegenerativeHealing (
  regenerativeHealing,
  RegenerativeHealing (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

regenerativeHealing :: TreacheryCard RegenerativeHealing
regenerativeHealing = treachery RegenerativeHealing Cards.regenerativeHealing

newtype RegenerativeHealing = RegenerativeHealing (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The villain heals double its stage; with no damage to heal, surge.
instance RunMessage RegenerativeHealing where
  runMessage msg t@(RegenerativeHealing attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      villain <- selectJust ActiveVillain
      damaged <- selectAny $ VillainWithId villain <> VillainWithAnyDamage
      if damaged
        then do
          stage <- getVillainStage villain
          heal villain (2 * stage)
          pure t
        else pure . RegenerativeHealing $ attrs & surgeL .~ True
    -- [star] Boost: the villain heals 2 damage
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      traverse_ (\villain -> heal villain 2) =<< selectOne ActiveVillain
      pure t
    _ -> RegenerativeHealing <$> runMessage msg attrs
