module Marvel.Hero.Heroes.PsylockeHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Payment ()
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword ()
import Marvel.Card
import Marvel.Game.Source ()
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner

psylockeHero :: HeroCard PsylockeHero
psylockeHero = hero PsylockeHero Cards.psylockeHero (HP $ Static 10) (HandSize 4) (Thw 1) (Atk 1) (Def 2)

newtype PsylockeHero = PsylockeHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PsylockeHero where
  getAbilities (PsylockeHero a) =
    [limitedWindowAbility a 1 (W.WindowOneOf [W.MakesBasicAttack W.When You, W.MakesBasicThwart W.When You, W.MakesBasicDefense W.When You AnyEnemy])
      Interrupt (IsSelf <> UpgradeExists (UpgradeControlledBy (IdentityWithId $ heroIdentityId a) <> UpgradeCanFlipPsiEnergy)) NoCost (runAbility a 1)]

instance RunMessage PsylockeHero where
  runMessage msg e@(PsylockeHero a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      upgrades <- selectList (UpgradeControlledBy (IdentityWithId you) <> UpgradeCanFlipPsiEnergy)
      chooseOne you [targeting u (flipPsiEnergy u) | u <- upgrades]
      pure e
    _ -> PsylockeHero <$> runMessage msg a
