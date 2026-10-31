module Marvel.Upgrade.Upgrades.WolverinesClaws where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

wolverinesClaws :: UpgradeCard WolverinesClaws
wolverinesClaws = upgrade WolverinesClaws Cards.wolverinesClaws

newtype WolverinesClaws = WolverinesClaws (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WolverinesClaws where
  getAbilities a = [ability a 1 HeroAction (OwnsThis <> CanPlayAttackEventWithClaws) ExhaustCost (runAbility a 1)]

instance RunMessage WolverinesClaws where
  runMessage msg u@(WolverinesClaws attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- playableClawsEvents you (toSource attrs)
      chooseOne you [targeting c do
        continuation <- capture $ cardSpecific attrs "play" (you, c)
        payCostThen you attrs (DamageCost $ fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef c) continuation
        | c <- cards]
      pure u
    CardSpecific (isTarget attrs -> True) "play" (fromJSON -> Success (you, card)) -> do
      createEffect Cards.wolverinesClaws attrs (EventEntity $ EventId $ unCardId $ pcCardId card)
      playCardIgnoringResourceCost you card
      pure u
    _ -> WolverinesClaws <$> runMessage msg attrs

newtype WolverinesClawsEffect = WolverinesClawsEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

wolverinesClawsEffect :: CardEffect WolverinesClawsEffect
wolverinesClawsEffect = effectWith WolverinesClawsEffect Cards.wolverinesClaws (modifiersL .~ [KeywordModifier Piercing, PlayedUsingWolverinesClaws])

-- A labeled attack can deal several instances of damage. Keep piercing for the whole
-- first actual attack, rather than consuming it after its first damaged enemy.
instance RunMessage WolverinesClawsEffect where
  runMessage msg e@(WolverinesClawsEffect attrs) = runQueueT $ case msg of
    CheckWindows ws
      | any firstAttackFinished ws -> e <$ disableEffect (effectId attrs)
    EventMessage eid ResolvedEvent | effectMatcher attrs == EventEntity eid -> e <$ disableEffect (effectId attrs)
    _ -> WolverinesClawsEffect <$> runMessage msg attrs
   where
    firstAttackFinished = \case
      W.Window W.After (W.PlayerAttackResolved (EventRef eid) _ _) -> effectMatcher attrs == EventEntity eid
      _ -> False
