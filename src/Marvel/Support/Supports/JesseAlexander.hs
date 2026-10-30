module Marvel.Support.Supports.JesseAlexander where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource.Cards qualified as Resources
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

jesseAlexander :: SupportCard JesseAlexander
jesseAlexander = support JesseAlexander Cards.jesseAlexander

newtype JesseAlexander = JesseAlexander (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities JesseAlexander where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage JesseAlexander where
  runMessage msg s@(JesseAlexander attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch $ CardIs Resources.connectionToTheWorldmind) <$> projectMap PlayerIdentityDiscard unDiscard you
      unless (null cards) $ chooseOne you [targeting c (shuffleIntoDeck you [c]) | c <- cards]
      drawCards you 1
      pure s
    _ -> JesseAlexander <$> runMessage msg attrs
