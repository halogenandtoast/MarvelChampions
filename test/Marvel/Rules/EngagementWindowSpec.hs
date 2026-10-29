module Marvel.Rules.EngagementWindowSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (windowAbility)
import Marvel.Ability.Types (AbilityType (..), HasAbilities (..))
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Matchers.Types hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (HasModifiersFor)
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import Marvel.Window qualified as W
import TestImport

-- A generic Anticipation-style interrupt, independent of the pack registry.
data EngagementProbe = EngagementProbe (Attrs Upgrade) [Ability]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasModifiersFor)

instance IsUpgrade EngagementProbe where
  toUpgradeAttrs (EngagementProbe attrs _) = attrs
instance HasAbilities EngagementProbe where
  getAbilities (EngagementProbe _ abilities) = abilities
instance RunMessage EngagementProbe where
  runMessage msg (EngagementProbe attrs abilities) =
    (\a -> EngagementProbe a abilities) <$> runMessage msg attrs

installInterrupt :: IdentityId -> GameAppT PlayerCard
installInterrupt p = do
  card <- putIntoPlay p "01008"
  attrs <- getsGame (toAttrs . fromMaybe (error "missing probe") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  let ab = windowAbility attrs 1 (W.EngagesMinion W.When (IdentityWithId p) AnyMinion) HeroInterrupt NoCost
        (Run [IdentityMessage p ReadiedIdentity])
  withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (EngagementProbe attrs [ab])
  pure card

spec :: Spec
spec = describe "engagement interrupt timing" do
  it "readies before quickstrike, while the existing After engagement response follows the attack" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      whenInterrupt <- installInterrupt p
      afterResponse <- putIntoPlay p "13017"
      resolve [IdentityMessage p ExhaustedIdentity]
      vulture <- revealNow p Minions.vulture
      offers (isAbilityOfCard whenInterrupt) `shouldReturn` True
      damageOf p `shouldReturn` 0
      choose "ready before engaging" (isAbilityOfCard whenInterrupt)
      exhaustedNow p `shouldReturn` False
      offers (isAbilityOfCard afterResponse) `shouldReturn` False
      choose "defend quickstrike with the newly readied hero" $ \case
        Defend (EnemyMinionId m) -> m == minionOf vulture
        _ -> False
      damageOf p `shouldReturn` 0
      exhaustedNow p `shouldReturn` True
      choose "Lie in Wait responds after quickstrike" (isAbilityOfCard afterResponse)
      pass
      minionDamageOf (minionOf vulture) `shouldReturn` Just 3
      upgradesWith "13017" `shouldReturn` []
