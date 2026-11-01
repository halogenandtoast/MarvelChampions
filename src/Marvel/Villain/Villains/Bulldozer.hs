module Marvel.Villain.Villains.Bulldozer where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attack ()
import Marvel.Cost ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id ()
import Marvel.Keyword
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question ()
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window ()

newtype Bulldozer = Bulldozer (Attrs Villain)
  deriving anyclass (IsVillain, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

bulldozer :: VillainCard Bulldozer
bulldozer = bulldozerA

bulldozerA :: VillainCard Bulldozer
bulldozerA = villain Bulldozer Cards.bulldozerA (Sch 1) (Atk 3) (HP $ PerPlayer 12)

bulldozerB :: VillainCard Bulldozer
bulldozerB = villainWith Bulldozer Cards.bulldozerB (Sch 2) (Atk 3) (HP $ PerPlayer 15) (stageL .~ 2)

-- | Forced Interrupt: when Bulldozer attacks, the attack gains overkill. Every attack of
-- his gains it, so he attacks with the keyword.
instance HasModifiersFor Bulldozer where
  getModifiersFor _ (VillainRef vid) (Bulldozer a) | vid == villainId a = pure [KeywordModifier Overkill]
  getModifiersFor _ _ _ = pure []

instance RunMessage Bulldozer where
  runMessage msg (Bulldozer attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      b <- versionBEnters attrs
      if b
        then advanceVillain ident >> pure (advanceVillainTo bulldozerB attrs)
        else Bulldozer <$> runMessage msg attrs
    _ -> Bulldozer <$> runMessage msg attrs
