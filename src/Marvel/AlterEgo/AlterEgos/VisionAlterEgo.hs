module Marvel.AlterEgo.AlterEgos.VisionAlterEgo where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Query ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.AlterEgo.Runner ()
import Marvel.Obligation.Cards qualified as Obligations
visionAlterEgo :: AlterEgoCard VisionAlterEgo
visionAlterEgo = alterEgo VisionAlterEgo Cards.visionAlterEgo (HP $ Static 11) (HandSize 5) (Rec 3) [Obligations.corruptedProgramming]

newtype VisionAlterEgo = VisionAlterEgo (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor VisionAlterEgo where
  getModifiersFor _ (IdentityRef you) (VisionAlterEgo a) | you == alterEgoIdentityId a = do
    dense <- isInMassForm you Upgrades.dense
    intangible <- isInMassForm you Upgrades.intangible
    pure $ [RecoveryModifier 2 | dense] <> [HandSizeModifier 1 | intangible]
  getModifiersFor _ _ _ = pure []

instance HasAbilities VisionAlterEgo where
  getAbilities a = [ability a 1 Setup IsSelf NoCost (runAbility a 1)]

instance RunMessage VisionAlterEgo where
  runMessage msg a@(VisionAlterEgo attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ putMassFormIntoPlay you
    _ -> VisionAlterEgo <$> runMessage msg attrs
