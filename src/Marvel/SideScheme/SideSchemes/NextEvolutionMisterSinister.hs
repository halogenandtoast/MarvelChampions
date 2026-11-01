module Marvel.SideScheme.SideSchemes.NextEvolutionMisterSinister where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
    ( AbilityType(ForcedInterrupt),
      HasAbilities(..),
      limitedWindowAbility )
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Keyword qualified as Keyword ()
import Marvel.Matchers
import Marvel.Modifier
    ( HasModifiersFor(getModifiersFor), Modifier(CannotTakeDamage) )
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.NextEvolution.Sinister

import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types


import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MisterSinisterSideScheme = MisterSinisterSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40146 :: SideSchemeCard MisterSinisterSideScheme
ne40146 = sideScheme (\a -> MisterSinisterSideScheme a {sideSchemeCrisis = True}) Cards.ne40146 (Static 3)

instance IsSideScheme MisterSinisterSideScheme where
  sideSchemeAbilities = getAbilities

instance RunMessage MisterSinisterSideScheme where
  runMessage msg card@(MisterSinisterSideScheme a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      sinister >>= traverse_ replaceVillainAttackWithScheme
      pure card
    _ -> MisterSinisterSideScheme <$> runMessage msg a

instance HasAbilities MisterSinisterSideScheme where
  getAbilities (MisterSinisterSideScheme a) =
    [limitedWindowAbility a 1 (W.EnemyWouldAttack (EnemyWithTitle "Mister Sinister") AnyIdentity) ForcedInterrupt NoCriteria NoCost (runAbility a 1)]

instance HasModifiersFor MisterSinisterSideScheme where
  getModifiersFor _ (VillainRef vid) _ = do
    target <- sinister
    pure [CannotTakeDamage | target == Just vid]
  getModifiersFor _ _ _ = pure []
