module Marvel.Minion.Minions.LadyDeathstrikeX23 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Window qualified as W ()
import Marvel.Boost (boostCount)
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

ladyDeathstrikeX23 :: MinionCard LadyDeathstrikeX23
ladyDeathstrikeX23 = minion (\a -> LadyDeathstrikeX23 (a `With` Nothing)) Cards.ladyDeathstrikeX23 (Sch 2) (Atk 2) (HP 5)

newtype LadyDeathstrikeX23 = LadyDeathstrikeX23 (Attrs Minion `With` Maybe IdentityId)
  deriving anyclass (HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion LadyDeathstrikeX23 where
  toMinionAttrs (LadyDeathstrikeX23 (a `With` _)) = a
  overMinionAttrs f (LadyDeathstrikeX23 (a `With` who)) = LadyDeathstrikeX23 (f a `With` who)

instance RunMessage LadyDeathstrikeX23 where
  runMessage msg m@(LadyDeathstrikeX23 (attrs `With` defeater)) = runQueueT $ case msg of
    MinionMessage mid (MinionDefeatedWithExcess source _ _) | mid == minionId attrs -> do
      you <- getSourceController source
      a <- runMessage msg attrs
      pure $ LadyDeathstrikeX23 (a `With` you)
    MinionMessage mid MinionDefeated | mid == minionId attrs -> do
      you <- maybe getActivePlayerId pure defeater
      chooseOnePromptedAbout you "$label.resolveWhenDefeated" attrs
        [targeting attrs (cardSpecific attrs "whenDefeated" you)]
      a <- runMessage msg attrs
      pure $ LadyDeathstrikeX23 (a `With` Just you)
    CardSpecific (isTarget attrs -> True) "whenDefeated" _ -> do
      discardTopOfEncounterDeckFor 1 attrs
      pure m
    WithDiscarded (isTarget attrs -> True) FromEncounterDeck cards -> do
      let n = sum $ map (boostCount . cdBoostIcons . getCardDef) cards
      for_ defeater $ \you -> when (n > 0) $ dealIndirectDamage you attrs n
      pure m
    _ -> (\a -> LadyDeathstrikeX23 (a `With` defeater)) <$> runMessage msg attrs
