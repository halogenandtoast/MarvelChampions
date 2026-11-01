module Marvel.Minion.Minions.FabianCortez where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity ()
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref ()
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

fabianCortez :: MinionCard FabianCortez
fabianCortez = minion (FabianCortez . (`With` Nothing)) Cards.fabianCortez (Sch 2) (Atk 2) (HP 4)

newtype FabianCortez = FabianCortez (Attrs Minion `With` Maybe IdentityId)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion FabianCortez where
  toMinionAttrs (FabianCortez (attrs `With` _)) = attrs
  overMinionAttrs f (FabianCortez (attrs `With` owner)) = FabianCortez (f attrs `With` owner)

instance RunMessage FabianCortez where
  runMessage msg e@(FabianCortez (attrs `With` defeatedBy)) = runQueueT $ case msg of
    MinionMessage mid (MinionDefeatedWithExcess source damage excess) | mid == minionId attrs -> do
      owner <- getSourceController source
      updated <- runMessage (MinionMessage mid $ MinionDefeatedWithExcess source damage excess) attrs
      pure $ FabianCortez (updated `With` owner)
    MinionMessage mid MinionDefeated | mid == minionId attrs -> do
      you <- maybe getYou pure defeatedBy
      discardTopOfDeck you 4
      (\a -> FabianCortez (a `With` Nothing)) <$> runMessage msg attrs
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      discardTopOfDeck you 4
      pure e
    _ -> (\a -> FabianCortez (a `With` defeatedBy)) <$> runMessage msg attrs
