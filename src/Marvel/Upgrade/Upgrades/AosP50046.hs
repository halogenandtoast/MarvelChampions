{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50046 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Keyword
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

aosP50046 :: UpgradeCard AosP50046
aosP50046 = upgrade AosP50046 Cards.aosP50046

newtype AosP50046 = AosP50046 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50046 where
  getAbilities (AosP50046 attrs) =
    [limitedWindowAbility attrs 1 (W.ResolvesAbility W.After (IdentityWithId $ upgradeController attrs) (CardWithTrait T.Preparation)) HeroResponse (OwnsThis <> MovableThreatExists <> UpgradeExists (UpgradeControlledBy (IdentityWithId $ upgradeController attrs) <> UpgradeOneOf [UpgradeWithCard (CardWithTitle "Assault"), UpgradeWithCard (CardWithTitle "Stealth")])) NoCost (runAbility attrs 1)]

instance RunMessage AosP50046 where
  runMessage msg card@(AosP50046 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      schemes <- selectList AnyScheme
      eligible <- filterM (\scheme -> (&&) <$> canMoveSchemeThreatFrom attrs scheme <*> (fmap (>0) $ getSchemeThreatAmount scheme)) schemes
      chooseOne you [targeting (schemeRef scheme) (cardSpecific attrs "secretAgentMove" (you,scheme)) | scheme <- eligible]
      pure card
    CardSpecific (isTarget attrs -> True) "secretAgentMove" (fromJSON -> Success (you,scheme)) -> do
      removable <- canMoveSchemeThreatFrom attrs scheme
      amount <- getSchemeThreatAmount scheme
      suit <- suitForm you
      when (removable && amount > 0 && isJust suit) do
        redirected <- elem RemovedThreatPlacedOnMinion <$> getModifiers (schemeRef scheme)
        case scheme of
          SchemeMainSchemeId sid -> mainSchemeStep sid (MainSchemeThreatRemoved (toSource attrs) 1)
          SchemeSideSchemeId sid -> sideSchemeStep sid (SideSchemeThreatRemoved (toSource attrs) 1)
        unless redirected $ placeSuitThreat you 1
      pure card
    _ -> AosP50046 <$> runMessage msg attrs
