module Marvel.Ally.Allies.SpDr where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Damage
import Marvel.Game.Source (cannotTakeDamageFrom, isBlanked)

spDr :: AllyCard SpDr
spDr = ally (SpDr . (`With` False)) Cards.spDr (Thw 2, 2) (Atk 1, 1) (HP 2)

-- The damage that defeated her must be consequential and strictly exceed her remaining HP.
newtype SpDr = SpDr (Attrs Ally `With` Bool)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAlly SpDr where
  toAllyAttrs (SpDr (attrs `With` _)) = attrs

instance IsRef SpDr where
  toRef = toRef . toAllyAttrs

instance HasModifiersFor SpDr
instance HasAbilities SpDr

instance RunMessage SpDr where
  runMessage msg a@(SpDr (attrs `With` excess)) = runQueueT $ case msg of
    AllyMessage aid (AllyDamaged source damage) | aid == allyId attrs -> do
      immune <- cannotTakeDamageFrom (AllyRef aid) source
      hp <- getModifiedHitPoints attrs
      let qualifies = not immune && not (allyTough attrs && preventedByTough damage)
            && damageSource damage == FromConsequential && damageAmount damage + allyDamage attrs > hp
      attrs' <- runMessage msg attrs
      pure $ SpDr (attrs' `With` qualifies)
    CheckWindows windows | excess && any defeated windows -> do
      blanked <- isBlanked attrs
      unless blanked $ insteadOf (== RemoveFromPlay (toTarget attrs)) (returnToHand attrs)
      SpDr . (`With` excess) <$> runMessage msg attrs
    _ -> SpDr . (`With` excess) <$> runMessage msg attrs
   where
    defeated = \case
      W.Window W.When (W.DefeatedAlly aid _) -> aid == allyId attrs
      _ -> False
