module Marvel.Support.Supports.Nc48003 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Support.Types
import Marvel.Support.Cards qualified as Cards

nc48003 :: SupportCard Nc48003
nc48003 = support Nc48003 Cards.nc48003

newtype Nc48003 = Nc48003 (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Nc48003 where
  getModifiersFor _ (IdentityRef you) (Nc48003 _) = do
    kurt <- selectAny (IdentityWithId you <> IdentityWithTitle "Kurt Wagner")
    pure [RecoveryModifier 1 | kurt]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Nc48003 where
  getAbilities a@(Nc48003 attrs) = [limitedWindowAbility a 1
    (W.UsesRecoveryAt W.After $ IdentityWithId $ supportController attrs)
    AlterEgoResponse OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage Nc48003 where
  runMessage msg a@(Nc48003 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      chooseOne you [targeting p (drawCards p 1) | p <- players]
      pure a
    _ -> Nc48003 <$> runMessage msg attrs
