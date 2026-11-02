module Marvel.Support.Supports.DrSinclairSilk where
import Marvel.Prelude
import Marvel.Id
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()
import Marvel.AlterEgo.Runner (getModifiedRecovery)
import Marvel.Identity.Types (Field(PlayerIdentitySide,PlayerIdentitySides), PlayerIdentitySide(..))
import Marvel.Projection
import Marvel.Resource
import Marvel.Status

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Support.Types

drSinclairSilk :: SupportCard DrSinclairSilk
drSinclairSilk = support DrSinclairSilk Cards.drSinclairSilk
newtype DrSinclairSilk = DrSinclairSilk (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage DrSinclairSilk where
  runMessage msg u@(DrSinclairSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      side <- project PlayerIdentitySide you
      sides <- project PlayerIdentitySides you
      case lookup side sides of
        Just (AlterEgoSide ae) -> heal you =<< getModifiedRecovery (toAttrs ae)
        _ -> pure ()
      statuses <- filterM (\status -> (>0) <$> getCharacterStatusCount (IdentityCharacter you) status) [StunnedStatus, ConfusedStatus, ToughStatus]
      chooseOne you (labeled "$label.done" (pure ()) : [labeled (statusLabel status) (removeStatus you status) | status <- statuses])
      pure u
    _ -> DrSinclairSilk <$> runMessage msg attrs

instance HasAbilities DrSinclairSilk where
  getAbilities u@(DrSinclairSilk _attrs) = [ability u 1 AlterEgoAction ForAnyPlayer (ExhaustCost <> ResourceCost (Just Mental)) (runAbility u 1)]
