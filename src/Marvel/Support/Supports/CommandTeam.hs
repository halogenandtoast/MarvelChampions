module Marvel.Support.Supports.CommandTeam (commandTeam, CommandTeam (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Matchers qualified as M
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

commandTeam :: SupportCard CommandTeam
commandTeam = supportWith CommandTeam Cards.commandTeam $ (usesL .~ 3) . (discardIfNoUsesL .~ True)

newtype CommandTeam = CommandTeam (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CommandTeam where
  getAbilities a = [ability a 1 Action
    (OwnsThis <> AllyExists (M.ExhaustedAlly <> AllyCanReady)) (ExhaustCost <> UseCost) (runAbility a 1)]

instance RunMessage CommandTeam where
  runMessage msg s@(CommandTeam attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      allies <- selectList (M.ExhaustedAlly <> AllyCanReady)
      unless (null allies) $ chooseOne ident [targeting ally (ready ally) | ally <- allies]
      pure s
    _ -> CommandTeam <$> runMessage msg attrs
