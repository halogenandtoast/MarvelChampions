module Marvel.Treachery.Treacheries.ToughItOut where

import Data.Maybe (maybeToList)
import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

toughItOut :: TreacheryCard ToughItOut
toughItOut = treachery ToughItOut Cards.toughItOut

newtype ToughItOut = ToughItOut (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ToughItOut where
  runMessage msg (ToughItOut attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId attrs -> do
      armadillo <- selectOne (MinionIs Minions.armadillo)
      villain <- selectOne ActiveVillain
      let enemies = map EnemyMinionId (maybeToList armadillo) <> map EnemyVillainId (maybeToList villain)
      given <- fmap sum $ for enemies $ \enemy -> do
        tough <- characterMatches (CharacterWithStatus ToughStatus) (toCharacter enemy)
        stacks <- elem CanHaveAnyNumberOfToughStatusCards <$> getModifiers (enemyRef enemy)
        enemyBecomesTough enemy
        pure (if not tough || stacks then 1 else 0 :: Int)
      pure . ToughItOut $ attrs & surgeL .~ (given <= 1)
    _ -> ToughItOut <$> runMessage msg attrs
