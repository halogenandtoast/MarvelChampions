module Marvel.Support.Supports.CommandCenter where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Keyword
import Marvel.Window qualified as W
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

commandCenter :: SupportCard CommandCenter
commandCenter = support CommandCenter Cards.commandCenter

newtype CommandCenter = CommandCenter (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CommandCenter where
  getAbilities (CommandCenter a) =
    [limitedWindowAbility a 1 (W.SideSchemeDefeatedByThwart AnySideScheme (SourceIsAlly AnyAlly))
      Response (OwnsThis <> EnemyExists AnyEnemy) ExhaustCost (runAbility a 1)]

instance RunMessage CommandCenter where
  runMessage msg e@(CommandCenter attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy
      pure e
    _ -> CommandCenter <$> runMessage msg attrs
