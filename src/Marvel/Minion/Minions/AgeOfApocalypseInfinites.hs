module Marvel.Minion.Minions.AgeOfApocalypseInfinites where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.Hp
import Marvel.Keyword hiding (Surge)
import Marvel.Keyword qualified as Keyword
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Modifier
import Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites (genePoolThreat)
import Marvel.Stats

newtype AoaInfinitesMinion = AoaInfinitesMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

infiniteSoldier :: MinionCard AoaInfinitesMinion
infiniteSoldier = minion AoaInfinitesMinion Cards.infiniteSoldier (Sch 1) (Atk 2) (HP 3)

instance HasModifiersFor AoaInfinitesMinion where
  getModifiersFor _ target (AoaInfinitesMinion a) | target == toRef a = do
    threat <- genePoolThreat
    pure $ [KeywordModifier Quickstrike | threat >= 3]
      <> [KeywordModifier Keyword.Surge | threat >= 6]
      <> [HitPointModifier 3 | threat >= 9]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaInfinitesMinion where
  runMessage msg (AoaInfinitesMinion a) = AoaInfinitesMinion <$> runMessage msg a
