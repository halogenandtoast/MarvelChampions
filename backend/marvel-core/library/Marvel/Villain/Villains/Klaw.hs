module Marvel.Villain.Villains.Klaw where

import Marvel.Prelude

import Marvel.Ability
import Marvel.GameValue
import Marvel.Hp
import Marvel.Message
import Marvel.Stats
import Marvel.Villain.Attrs
import Marvel.Villain.Cards qualified as Cards

newtype Klaw = Klaw VillainAttrs
  deriving anyclass IsVillain
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

klaw :: VillainCard Klaw
klaw = villain Klaw Cards.klaw (Sch 2) (Atk 0) (HP $ PerPlayer 12)

instance HasAbilities Klaw where
  getAbilities _ = []

instance RunMessage Klaw where
  runMessage msg (Klaw attrs) = Klaw <$> runMessage msg attrs
