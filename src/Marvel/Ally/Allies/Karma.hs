module Marvel.Ally.Allies.Karma where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Game.Source (getAllyControlSource)
import Marvel.Query
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

karma :: AllyCard Karma
karma = ally (Karma . (`With` Nothing)) Cards.karma (Thw 0, 1) (Atk 0, 1) (HP 1)

newtype Karma = Karma (Attrs Ally `With` Maybe AllyId)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
  deriving anyclass HasModifiersFor

instance IsAlly Karma where
  toAllyAttrs (Karma (attrs `With` _)) = attrs

instance IsRef Karma where
  toRef (Karma (attrs `With` _)) = toRef attrs

instance HasAbilities Karma where
  getAbilities a@(Karma (attrs `With` _)) =
    [limitedWindowAbility a 1 (W.PlayedFromHand (IdentityWithId $ allyController attrs) $ CardIs Cards.karma)
      Response (OwnsThis <> MinionExists (NotMinion $ MinionWithTrait Trait.Elite)) NoCost (runAbility a 1)]

instance RunMessage Karma where
  runMessage msg a@(Karma (attrs `With` host)) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      minions <- selectList (NotMinion $ MinionWithTrait Trait.Elite)
      chooseOne you [targeting (MinionRef mid) (cardSpecific attrs "control" (you, mid)) | mid <- minions]
      pure a
    CardSpecific (isTarget attrs -> True) "control" (fromJSON -> Success (you, mid)) -> do
      takeControlOfMinionFrom attrs you mid 2
      pure $ Karma (attrs `With` Just (AllyId $ unMinionId mid))
    RemoveFromPlayAfterWindow target | target == toRef attrs -> a <$ restoreHost
    RemoveFromGame target | target == toRef attrs -> a <$ restoreHost
    ReturnToHand target | target == toRef attrs -> a <$ restoreHost
    AddToVictoryDisplay target | target == toRef attrs -> a <$ restoreHost
    _ -> (\attrs' -> Karma (attrs' `With` host)) <$> runMessage msg attrs
   where
    restoreHost = for_ host $ \aid -> do
      leaving <- isQueued $ \case
        DefeatedLeavesPlay target -> target == AllyRef aid
        RemoveFromPlay target -> target == AllyRef aid
        RemoveFromPlayAfterWindow target -> target == AllyRef aid
        RemoveFromGame target -> target == AllyRef aid
        ReturnToHand target -> target == AllyRef aid
        AddToVictoryDisplay target -> target == AllyRef aid
        _ -> False
      present <- selectAny $ AllyWithId aid
      when (present && not leaving) do
        source <- getAllyControlSource aid
        when (isNothing source || source == Just (toSource attrs)) $ restoreControlledMinion aid
