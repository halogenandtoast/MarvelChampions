module Marvel.Support.Supports.WhiteHotRoom where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

whiteHotRoom :: SupportCard WhiteHotRoom
whiteHotRoom = support WhiteHotRoom Cards.whiteHotRoom

newtype WhiteHotRoom = WhiteHotRoom (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WhiteHotRoom where
  getAbilities (WhiteHotRoom a) = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage WhiteHotRoom where
  runMessage msg e@(WhiteHotRoom a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      force <- selectAny (UpgradeNamed "Phoenix Force" <> upgradeControlledBy you)
      hurt <- identityMatches healableIdentity you
      chooseOne you $ [labeled "$label.phoenixPowerOne" (addPhoenixPower you 1) | force]
        <> [labeled "$label.phoenixHealTwo" (heal you 2) | hurt]
      pure e
    _ -> WhiteHotRoom <$> runMessage msg a
