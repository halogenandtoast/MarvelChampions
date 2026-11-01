module Marvel.Support.Supports.IPAC where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

ipac :: SupportCard IPAC
ipac = support IPAC Cards.ipac

newtype IPAC = IPAC (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IPAC where
  getAbilities a = [ability a 1 HeroAction OwnsThis (ExhaustCost <> DealEncounterCardToPlayerCost AnyIdentity) (runAbility a 1)]

instance RunMessage IPAC where
  runMessage msg e@(IPAC a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ payment -> do
      for_ (paymentTarget payment) $ \case
        IdentityRef p -> drawCards p 2
        _ -> pure ()
      pure e
    _ -> IPAC <$> runMessage msg a
