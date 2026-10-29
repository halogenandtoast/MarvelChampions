module Marvel.AlterEgo.AlterEgos.NebulaAlterEgo (nebulaAlterEgo, NebulaAlterEgo (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait (Trait (Technique))
import Marvel.Window qualified as W

nebulaAlterEgo :: AlterEgoCard NebulaAlterEgo
nebulaAlterEgo = alterEgo NebulaAlterEgo Cards.nebulaAlterEgo (HP $ Static 9) (HandSize 6) (Rec 3) [Obligations.inferiorityComplex]

newtype NebulaAlterEgo = NebulaAlterEgo (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities NebulaAlterEgo where
  getAbilities ae@(NebulaAlterEgo attrs) =
    [ label "$label.cyberneticUpgrades" $ limited (PerRound 1) $
        limitedWindowAbility ae 2
          (W.PlaysCard W.After (IdentityWithId $ alterEgoIdentityId attrs) (CardWithType UpgradeType <> CardWithTrait Technique))
          Response IsSelf NoCost (runAbility ae 2)
    ]

instance RunMessage NebulaAlterEgo where
  runMessage msg ae@(NebulaAlterEgo attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 2 _ _ -> ae <$ drawCards ident 2
    _ -> NebulaAlterEgo <$> runMessage msg attrs
