module Marvel.Upgrade.Upgrades.CryokineticPerception where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Upgrade.Types

cryokineticPerception :: UpgradeCard CryokineticPerception
cryokineticPerception = upgrade CryokineticPerception Cards.cryokineticPerception

newtype CryokineticPerception = CryokineticPerception (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CryokineticPerception where
  getAbilities a@(CryokineticPerception attrs) = [limitedWindowAbility a 1
    (W.ResolvesAbility W.After (IdentityWithId $ upgradeController attrs) $ CardIs Cards.icemanHero)
    HeroResponse OwnsThis ExhaustCost (runAbility a 1)]
instance RunMessage CryokineticPerception where
  runMessage msg a@(CryokineticPerception attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      before <- unHand <$> project PlayerIdentityHand you
      drawCards you 1
      cardSpecific attrs "checkDraw" (you, map toCardId before)
      pure a
    CardSpecific (isTarget attrs -> True) "checkDraw" (fromJSON -> Success (you, before :: [CardId])) -> do
      after <- unHand <$> project PlayerIdentityHand you
      when (any (\c -> toCardId c `notElem` before && cardMatch (CardWithTrait Trait.Ice) c) after) $ do
        icemen <- selectList (HeroIdentity <> IdentityWithTitle "Iceman")
        traverse_ ready icemen
      pure a
    _ -> CryokineticPerception <$> runMessage msg attrs
