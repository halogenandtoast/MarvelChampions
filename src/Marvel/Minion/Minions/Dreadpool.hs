module Marvel.Minion.Minions.Dreadpool where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Card
import Marvel.Game.Source
import Marvel.Question ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Stats ()

dreadpool :: MinionCard Dreadpool
dreadpool = minion (\a -> Dreadpool (a `With` Nothing)) Cards.dreadpool (Sch 2) (Atk 2) (HP 3)

newtype Dreadpool = Dreadpool (Attrs Minion `With` Maybe IdentityId)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion Dreadpool where
  toMinionAttrs (Dreadpool (attrs `With` _)) = attrs
  overMinionAttrs f (Dreadpool (attrs `With` owner)) = Dreadpool (f attrs `With` owner)

instance RunMessage Dreadpool where
  runMessage msg m@(Dreadpool (attrs `With` defeater)) = runQueueT $ case msg of
    MinionMessage mid (MinionDefeatedWithExcess source _ _) | mid == minionId attrs -> do
      who <- getSourceController source
      updated <- runMessage msg attrs
      pure $ Dreadpool (updated `With` who)
    MinionMessage mid MinionDefeated | mid == minionId attrs -> do
      who <- maybe getActivePlayerId pure defeater
      chooseOnePromptedAbout who "$label.resolveWhenDefeated" attrs
        [targeting attrs $ cardSpecific attrs "whenDefeated" who]
      updated <- runMessage msg attrs
      pure $ Dreadpool (updated `With` Just who)
    CardSpecific (isTarget attrs -> True) "whenDefeated" (fromJSON -> Success who) -> do
      let physical = MkEncounterCard (CardId $ unMinionId $ minionId attrs) (getCardDef attrs)
      afterMatching (\case ResolveMinionLeavingPlay mid _ -> mid == minionId attrs; _ -> False) $
        dealFoundEncounterCard who physical
      pure m
    _ -> do
      engagesFirstPlayer msg attrs
      (\updated -> Dreadpool (updated `With` defeater)) <$> runMessage msg attrs
