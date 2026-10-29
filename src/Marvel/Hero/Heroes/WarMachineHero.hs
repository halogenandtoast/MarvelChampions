module Marvel.Hero.Heroes.WarMachineHero (warMachineHero, WarMachineHero (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Question (runAbility)
import Marvel.Stats
import Marvel.Window qualified as W

warMachineHero :: HeroCard WarMachineHero
warMachineHero = hero WarMachineHero Cards.warMachineHero (HP $ Static 10) (HandSize 5) (Thw 1) (Atk 2) (Def 2)

newtype WarMachineHero = WarMachineHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities WarMachineHero where
  getAbilities a@(WarMachineHero attrs) =
    [ limitedWindowAbility a 1
        (W.IdentityChangedToForm W.After $ IdentityWithId (heroIdentityId attrs) <> HeroIdentity)
        Response IsSelf NoCost (runAbility a 1) ]

instance RunMessage WarMachineHero where
  runMessage msg h@(WarMachineHero attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      placeCounters ident 5
      pure h
    _ -> WarMachineHero <$> runMessage msg attrs
