module Marvel.Support.Supports.AlbertMoonSilk where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Support.Types

albertMoonSilk :: SupportCard AlbertMoonSilk
albertMoonSilk = support AlbertMoonSilk Cards.albertMoonSilk
newtype AlbertMoonSilk = AlbertMoonSilk (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage AlbertMoonSilk where
  runMessage msg u@(AlbertMoonSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- tuckedEncounterCards you
      chooseOne you [labeled "$label.silkTuckTop" (tuckTopEncounterCard you), labeled "$label.heal" (heal you $ fromIntegral $ length cards)]
      pure u
    _ -> AlbertMoonSilk <$> runMessage msg attrs

instance HasAbilities AlbertMoonSilk where
  getAbilities u@(AlbertMoonSilk _attrs) = [ability u 1 AlterEgoAction OwnsThis ExhaustCost (runAbility u 1)]
