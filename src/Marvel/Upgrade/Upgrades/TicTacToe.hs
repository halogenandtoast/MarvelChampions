module Marvel.Upgrade.Upgrades.TicTacToe where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

ticTacToe :: UpgradeCard TicTacToe
ticTacToe = upgradeWith TicTacToe Cards.ticTacToe (\attrs -> attrs {upgradeResourceSlots = [Energy, Energy, Energy, Mental, Mental, Mental, Physical, Physical, Physical]})

newtype TicTacToe = TicTacToe (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TicTacToe where
  getAbilities a@(TicTacToe attrs) =
    [ label (countLabel "metagameSlot" $ fromIntegral $ slot + 1) $
        ability a (fromIntegral $ slot + 1) HeroAction (OwnsThis <> CharacterExists (CharacterWithAnyDamage <> CharacterCanBeHealed))
          (ResourceCost $ Just resource) (runAbility a $ fromIntegral $ slot + 1)
    | (slot, resource) <- zip [0 ..] (upgradeResourceSlots attrs)
    , slot `notElem` upgradeOccupiedSlots attrs
    ]

instance RunMessage TicTacToe where
  runMessage msg e@(TicTacToe attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) idx _ payment -> do
      let slot = fromIntegral idx - 1
      for_ (atMay (upgradeResourceSlots attrs) slot) $ \resource -> whenM (paidWith resource payment) do
          characters <- selectList (CharacterWithAnyDamage <> CharacterCanBeHealed)
          chooseOne you [targeting (characterRef character) do
            moveDamageToUpgradeSlot (upgradeId attrs) character slot
            cardSpecific attrs "completed" ()
            | character <- characters]
      pure e
    CardSpecific (isTarget attrs -> True) "completed" _ -> do
      when (hasLine $ upgradeOccupiedSlots attrs) do
        let amount = fromIntegral $ length $ upgradeOccupiedSlots attrs
        chooseDamage (upgradeController attrs) attrs (toDamage amount FromAbility) AnyEnemy
        removeFromPlay attrs
      pure e
    _ -> TicTacToe <$> runMessage msg attrs

-- Slots are zero-based in row-major order, as on the printed diagram.
hasLine :: [Int] -> Bool
hasLine occupied = any (all (`elem` occupied))
  [[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [2,4,6]]
