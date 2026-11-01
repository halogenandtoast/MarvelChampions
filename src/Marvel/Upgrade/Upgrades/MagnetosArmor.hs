module Marvel.Upgrade.Upgrades.MagnetosArmor where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types qualified as R
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

magnetosArmor :: UpgradeCard MagnetosArmor
magnetosArmor = upgrade MagnetosArmor Cards.magnetosArmor

newtype MagnetosArmor = MagnetosArmor (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MagnetosArmor where
  getAbilities (MagnetosArmor a) = [limitedWindowAbility a 1 (W.MagneticPullSucceeded (IdentityWithId $ upgradeController a))
    Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage MagnetosArmor where
  runMessage msg e@(MagnetosArmor attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let icons = concat [printedResources (getCardDef c) | W.MagneticPullResolved who cards <- windows, who == you, c <- cards]
          mods = [ThwartModifier 1 | R.Mental `elem` icons] <> [AttackModifier 1 | R.Physical `elem` icons] <> [DefenseModifier 1 | R.Energy `elem` icons]
      magnetos <- selectList (HeroIdentity <> IdentityWithTitle "Magneto")
      for_ magnetos $ \p -> lastingModifiers attrs (IdentityEntity $ IdentityWithId p) mods True
      pure e
    _ -> MagnetosArmor <$> runMessage msg attrs
