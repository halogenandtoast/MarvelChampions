module Marvel.Minion.Minions.NextEvolutionNeStryfe where

import Marvel.Minion.Runner
import Marvel.Prelude
import Marvel.Card ()
import Marvel.Entity ()
import Marvel.Id ()
import Marvel.Game.Source
import Marvel.NextEvolution.Stryfe
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types ()
import Marvel.Ability (HasAbilities)
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted ()

newtype NeStryfeMinion = NeStryfeMinion (Attrs Minion `With` Maybe IdentityId)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40174 :: MinionCard NeStryfeMinion
ne40174 = minion (\a -> NeStryfeMinion (a `With` Nothing)) Cards.ne40174 (Sch 1) (Atk 1) (HP 4)

instance IsMinion NeStryfeMinion where
  toMinionAttrs (NeStryfeMinion (a `With` _)) = a
  overMinionAttrs f (NeStryfeMinion (a `With` who)) = NeStryfeMinion (f a `With` who)

instance RunMessage NeStryfeMinion where
  runMessage msg e@(NeStryfeMinion (a `With` who)) = runQueueT $ case msg of
    MinionMessage mid (MinionDefeatedWithExcess source _ _) | mid == minionId a -> do
      you <- getSourceController source
      attrs <- runMessage msg a
      pure $ NeStryfeMinion (attrs `With` you)
    MinionMessage mid MinionDefeated | mid == minionId a -> do
      n <- maybe (pure 0) handTypeCount who
      if n < 3 then shuffleMinionIntoEncounterDeck mid >> pure e
        else (\attrs -> NeStryfeMinion (attrs `With` who)) <$> runMessage msg a
    _ -> (\attrs -> NeStryfeMinion (attrs `With` who)) <$> runMessage msg a
