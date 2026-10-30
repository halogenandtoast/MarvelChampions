module Marvel.Rules.EventResolutionSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (limitedWindowAbility)
import Marvel.Ability.Types (AbilityType (Response), HasAbilities (..))
import Marvel.Cost.Types (Cost (NoCost, RevealEncounterTreacheryCost))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Criteria (Criteria (NoCriteria))
import Marvel.Matchers.Types (IdentityMatcher (IdentityWithId))
import Marvel.Modifier (HasModifiersFor)
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import Marvel.Window qualified as W
import TestImport

-- A response to a fully resolved interrupt/response event, independent of the new heroes.
data EventResolutionProbe = EventResolutionProbe (Attrs Upgrade) [Ability]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasModifiersFor)

instance IsUpgrade EventResolutionProbe where
  toUpgradeAttrs (EventResolutionProbe attrs _) = attrs
instance HasAbilities EventResolutionProbe where
  getAbilities (EventResolutionProbe _ abilities) = abilities
instance RunMessage EventResolutionProbe where
  runMessage msg (EventResolutionProbe attrs abilities) =
    (\a -> EventResolutionProbe a abilities) <$> runMessage msg attrs

installProbe :: IdentityId -> GameAppT PlayerCard
installProbe p = do
  card <- putIntoPlay p "01008"
  attrs <- getsGame (toAttrs . fromMaybe (error "missing probe") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  let ab = limitedWindowAbility attrs 1 (W.ResolvesEventAbility W.After (IdentityWithId p) AnyCard)
        Response NoCriteria NoCost (Run [IdentityMessage p ReadiedIdentity])
  withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (EventResolutionProbe attrs [ab])
  pure card

spec :: Spec
spec = describe "interrupt and response event resolution" do
  it "opens after an interrupt is paid, resolved and discarded, with its physical event ID" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      probe <- installProbe p
      event <- putInHand p "09021"
      resolve [IdentityMessage p ExhaustedIdentity]
      runNow $ damageIdentity p GameRef (toDamage 3 FromAbility)
      offers (isAbilityOfCard probe) `shouldReturn` False
      playWith event []
      offers (isAbilityOfCard probe) `shouldReturn` True
      discardOf p >>= (`shouldSatisfy` elem event)
      windows <- getCurrentWindows
      [(who, eid, toCardCode def) | W.Window W.After (W.EventAbilityResolved who eid def) <- windows]
        `shouldSatisfy` elem (p, EventId $ unCardId $ pcCardId event, "09021")
      choose "respond after the event resolved" (isAbilityOfCard probe)
      exhaustedNow p `shouldReturn` False
      pass
      damageOf p `shouldReturn` 2

  it "does not open for an action event" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      probe <- installProbe p
      clearHand p
      event <- putInHand p "01005"
      a <- putInHand p "01088"
      b <- putInHand p "01089"
      playWith event [a, b]
      chooseVillain
      villainDamageNow `shouldReturn` 8
      offers (isAbilityOfCard probe) `shouldReturn` False

  it "matches the event's controller and timing" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        q = player2 tg
    inGame tg do
      card <- putInHand p "09021"
      let window = W.Window W.After (W.EventAbilityResolved p (EventId $ unCardId $ pcCardId card) (getCardDef card))
      W.windowMatches (W.ResolvesEventAbility W.After (IdentityWithId p) AnyCard) window GameRef `shouldReturn` True
      W.windowMatches (W.ResolvesEventAbility W.When (IdentityWithId p) AnyCard) window GameRef `shouldReturn` False
      W.windowMatches (W.ResolvesEventAbility W.After (IdentityWithId q) AnyCard) window GameRef `shouldReturn` False

  describe "revealing a treachery as a cost" do
    it "fully resolves the revealed card before the event's damage" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        probe <- putIntoPlay p "01008"
        original <- putInHand p "01005"
        let event = original {pcCardDef = (getCardDef original) {cdCost = Just 0, cdAbilityCost = RevealEncounterTreacheryCost}}
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
          PlayerIdentity a {playerIdentityHand = Hand [event]}
        _ <- setEncounterDeck [Minions.shocker, Treacheries.caughtOffGuard, Minions.vulture] []
        playWith event []
        villainDamageNow `shouldReturn` 0
        choose "resolve the treachery's discard before attacking" (isTargetChoice $ upgradeIdOf probe)
        chooseVillain
        villainDamageNow `shouldReturn` 8

    it "does not resolve the event when no treachery can be revealed" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        original <- putInHand p "01005"
        let event = original {pcCardDef = (getCardDef original) {cdCost = Just 0, cdAbilityCost = RevealEncounterTreacheryCost}}
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
          PlayerIdentity a {playerIdentityHand = Hand [event]}
        _ <- setEncounterDeck [Minions.shocker, Minions.vulture] []
        playWith event []
        villainDamageNow `shouldReturn` 0
        handOf p >>= (`shouldSatisfy` elem event)

  describe "event maximums per triggering window" do
    it "shares a maximum across copies and nested resolutions, and resets for the next trigger" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        a <- putInHand p "09021"
        b <- putInHand p "09021"
        let limited card = card {pcCardDef = (getCardDef card) {cdLimit = Just (MaxPerWindow 1)}}
            first = limited a
            second = limited b
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) ->
          PlayerIdentity attrs {playerIdentityHand = Hand [first, second]}
        runNow $ damageIdentity p GameRef (toDamage 3 FromAbility)
        playWith first []
        offers (isPlay second) `shouldReturn` False
        pass
        damageOf p `shouldReturn` 2
        runNow $ damageIdentity p GameRef (toDamage 3 FromAbility)
        offers (isPlay second) `shouldReturn` True
        playWith second []
        pass
        damageOf p `shouldReturn` 4
