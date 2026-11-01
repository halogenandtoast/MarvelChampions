module Marvel.Obligation.Obligations.OldGrievances where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
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
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

oldGrievances :: ObligationCard OldGrievances
oldGrievances = obligation OldGrievances Cards.oldGrievances

newtype OldGrievances = OldGrievances (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities OldGrievances where
  getAbilities (OldGrievances a) = case obligationOwner a of
    Nothing -> []
    Just you ->
      [limitedWindowAbility a 1 (W.UsesAbility W.After (IdentityWithId you) 1) ForcedResponse NoCriteria NoCost (runAbility a 1)
      ,ability a 2 AlterEgoAction (SelfMatches $ IdentityWithId you) ExhaustCost (runAbility a 2)]

instance RunMessage OldGrievances where
  runMessage msg e@(OldGrievances attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (concatMap (\case W.MagneticPullResolved you cards -> [(you,cards)]; W.MagneticPullFailed you cards -> [(you,cards)]; _ -> []) windows) $ \(you, cards) -> damageIdentity you attrs (toDamage (fromIntegral $ length cards) FromAbility)
      pure e
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> e <$ removeFromPlay attrs
    _ -> OldGrievances <$> runMessage msg attrs
