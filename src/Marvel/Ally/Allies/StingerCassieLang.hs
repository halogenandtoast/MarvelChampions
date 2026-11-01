module Marvel.Ally.Allies.StingerCassieLang (
  stingerCassieLang,
  StingerCassieLang (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Modifier ()
import Marvel.Ref ()

stingerCassieLang :: AllyCard StingerCassieLang
stingerCassieLang = ally StingerCassieLang Cards.stingerCassieLang (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype StingerCassieLang = StingerCassieLang (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Stinger does not count against your ally limit.
instance HasModifiersFor StingerCassieLang where
  getModifiersFor _ (AllyRef aid) (StingerCassieLang a)
    | aid == allyId a = pure [NotCountedAgainstAllyLimit]
  getModifiersFor _ _ _ = pure []

instance RunMessage StingerCassieLang where
  runMessage msg (StingerCassieLang attrs) = StingerCassieLang <$> runMessage msg attrs
