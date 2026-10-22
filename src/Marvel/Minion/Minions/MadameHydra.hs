module Marvel.Minion.Minions.MadameHydra (
  madameHydra,
  MadameHydra (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Window

madameHydra :: MinionCard MadameHydra
madameHydra = minion MadameHydra Cards.madameHydra (Sch 2) (Atk 2) (HP 6)

newtype MadameHydra = MadameHydra (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MadameHydra where
  getAbilities (MadameHydra a) =
    [ windowAbility
        a
        1
        (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity)
        ForcedResponse
        NoCost
        $ RunAbility (toTarget a) 1
    ]

-- | Place 2 threat on the Legions of Hydra side scheme; the first player picks one if
-- both copies are in play.
placeThreatOnLegions :: (ReverseQueue m) => m ()
placeThreatOnLegions = do
  legions <- selectList (SideSchemeIs Cards.legionsOfHydra)
  firstPlayer <- selectJust FirstPlayer
  unless (null legions) $
    chooseOrRunOne
      firstPlayer
      [targeting sid (placeThreat sid 2) | sid <- legions]

instance RunMessage MadameHydra where
  runMessage msg e@(MadameHydra attrs) = runQueueT $ case msg of
    MinionMessage ident msg' | ident == minionId attrs -> case msg' of
      -- she cannot take damage while the Legions of Hydra side scheme is in play
      MinionDamaged _ _ -> do
        protected <- selectAny (SideSchemeIs Cards.legionsOfHydra)
        if protected then pure e else MadameHydra <$> runMessage msg attrs
      -- after she schemes: it follows the threat she places
      MinionSchemed -> do
        e' <- MadameHydra <$> runMessage msg attrs
        placeThreatOnLegions
        pure e'
      _ -> MadameHydra <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      placeThreatOnLegions
      pure e
    _ -> MadameHydra <$> runMessage msg attrs
