module Marvel.Minion.Minions.Harpoon where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attack
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

harpoon :: MinionCard Harpoon
harpoon = minion Harpoon Cards.harpoon (Sch 1) (Atk 1) (HP 6)

newtype Harpoon = Harpoon (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Harpoon where
  getModifiersFor _ target (Harpoon attrs) | target == toRef attrs = case minionAttacking attrs of
    Just attack -> do
      aerial <- characterMatches (CharacterWithTrait Trait.Aerial) (attackCharacter attack)
      pure [AttackModifier 1 | aerial]
    Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage Harpoon where
  runMessage msg a@(Harpoon attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      discardFromDeckUntil you (CardWithType EventType) attrs
      pure a
    WithDiscardedMatch (isTarget attrs -> True) FromDeck (PlayerCard card) -> do
      let cost = fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card
      dealIndirectDamage (minionEngagedIdentity attrs) attrs cost
      pure a
    _ -> Harpoon <$> runMessage msg attrs
