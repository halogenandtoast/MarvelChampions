module Marvel.Villain.Villains.Piledriver where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attack ()
import Marvel.Cost ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question ()
import Marvel.Ref ()
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window ()

newtype Piledriver = Piledriver (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

piledriver :: VillainCard Piledriver
piledriver = piledriverA

piledriverA :: VillainCard Piledriver
piledriverA = villain Piledriver Cards.piledriverA (Sch 2) (Atk 2) (HP $ PerPlayer 11)

piledriverB :: VillainCard Piledriver
piledriverB = villainWith Piledriver Cards.piledriverB (Sch 2) (Atk 3) (HP $ PerPlayer 14) (stageL .~ 2)

-- | Retaliate 1 (printed). When Piledriver schemes, the threat goes on his side scheme.
instance RunMessage Piledriver where
  runMessage msg (Piledriver attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      b <- versionBEnters attrs
      if b
        then advanceVillain ident >> pure (advanceVillainTo piledriverB attrs)
        else Piledriver <$> runMessage msg attrs
    _ -> Piledriver <$> runMessage msg attrs
