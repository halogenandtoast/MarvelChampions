module Marvel.Support.Supports.WontStayDown where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

wontStayDown :: SupportCard WontStayDown
wontStayDown = support WontStayDown Cards.wontStayDown

newtype WontStayDown = WontStayDown (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WontStayDown where
  getAbilities a = [ability a 1 AlterEgoAction
    (OwnsThis <> ExtendedCardExists (InDiscardOf You $ BasicCardMatches $ CardWithType AllyType <> CardMatchesAny [CardWithTrait T.XForce, CardWithTrait T.XMen]))
    (DiscardCost $ toTarget a) (runAbility a 1)]

instance RunMessage WontStayDown where
  runMessage msg e@(WontStayDown attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardWithType AllyType <> CardMatchesAny [CardWithTrait T.XForce, CardWithTrait T.XMen])
      chooseOne you [targeting c $ addToHand you c | c <- cards]
      pure e
    _ -> WontStayDown <$> runMessage msg attrs
