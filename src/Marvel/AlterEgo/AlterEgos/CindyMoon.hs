module Marvel.AlterEgo.AlterEgos.CindyMoon where
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
import Marvel.Silk.Helpers

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

cindyMoon :: AlterEgoCard CindyMoon
cindyMoon = alterEgo CindyMoon Cards.cindyMoon (HP $ Static 10) (HandSize 6) (Rec 3) [Cards.silkSenseOverloadSilk]
newtype CindyMoon = CindyMoon (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage CindyMoon where
  runMessage msg u@(CindyMoon attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseTuckedCard you (\card -> discardTuckedEncounterCard you card >> drawCards you 2)
      pure u
    _ -> CindyMoon <$> runMessage msg attrs

instance HasAbilities CindyMoon where
  getAbilities u@(CindyMoon _attrs) = [limited (PerRound 1) $ ability u 1 AlterEgoAction (IsSelf <> HasTuckedEncounterCard) NoCost (runAbility u 1)]
