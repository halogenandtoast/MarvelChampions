module Marvel.Upgrade.Upgrades.IceSlide where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Upgrade.Types

iceSlide :: UpgradeCard IceSlide
iceSlide = upgrade IceSlide Cards.iceSlide

newtype IceSlide = IceSlide (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor IceSlide where
  getModifiersFor _ (IdentityRef you) _ = do
    iceman <- selectAny (IdentityWithId you <> IdentityWithTitle "Iceman" <> HeroIdentity)
    pure [m | iceman, m <- [ThwartModifier 1,AttackModifier 1,DefenseModifier 1,TraitModifier Trait.Aerial]]
  getModifiersFor _ _ _ = pure []
instance HasAbilities IceSlide where
  getAbilities a@(IceSlide attrs) = [limitedWindowAbility a 1
    (W.IdentityChangedToForm W.After $ IdentityWithId $ upgradeController attrs)
    ForcedResponse (OwnsThis <> SelfMatches AlterEgoIdentity) NoCost (runAbility a 1)]
instance RunMessage IceSlide where
  runMessage msg a@(IceSlide attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      case toCard attrs of
        PlayerCard c -> removeFromGame attrs >> shuffleIntoDeck you [c]
        _ -> pure ()
      pure a
    _ -> IceSlide <$> runMessage msg attrs
