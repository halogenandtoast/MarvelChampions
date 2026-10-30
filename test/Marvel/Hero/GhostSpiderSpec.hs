module Marvel.Hero.GhostSpiderSpec (spec, ghostSpider, asGhost, attackVillain, isReflexes) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Phase (Phase (..))
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

ghostSpider :: IO TestGame
ghostSpider = testGame ["ghost-spider"]

asGhost :: IdentityId -> GameAppT ()
asGhost p = clearHand p >> changeForm p >> pass

attackVillain :: IdentityId -> GameAppT ()
attackVillain p = do
  choose "basic attack" (isHeroAttack p)
  chooseVillain

isReflexes :: Choice -> Bool
isReflexes = isNamedAbility "Dizzying Reflexes"

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . playerIdentityDeck . toAttrs <$> getPlayer p

isGwenAction :: IdentityId -> Choice -> Bool
isGwenAction p = \case
  UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1
  _ -> False

spec :: Spec
spec = describe "Ghost-Spider" do
  describe "Dizzying Reflexes" do
    it "readies after an interrupt event resolves, once per phase" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        resolve [IdentityMessage p ExhaustedIdentity]
        first <- putInHand p "09021"
        second <- putInHand p "09021"
        runNow $ damageIdentity p GameRef (toDamage 2 FromAbility)
        playWith first []
        exhaustedNow p `shouldReturn` True
        choose "Dizzying Reflexes" isReflexes
        exhaustedNow p `shouldReturn` False
        pass
        resolve [IdentityMessage p ExhaustedIdentity]
        runNow $ damageIdentity p GameRef (toDamage 2 FromAbility)
        playWith second []
        offers isReflexes `shouldReturn` False
        pass
        exhaustedNow p `shouldReturn` True

    it "readies after a response event, after its damage resolves" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27002"
        energy <- putInHand p "01088"
        attackVillain p
        offers isReflexes `shouldReturn` False
        playWith event [energy]
        chooseVillain
        villainDamageNow `shouldReturn` 8
        choose "Dizzying Reflexes after Ghost Kick" isReflexes
        exhaustedNow p `shouldReturn` False

    it "does not respond to an action event or a basic power alone" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "01005"
        energy <- putInHand p "01088"
        genius <- putInHand p "01089"
        resolve [IdentityMessage p ExhaustedIdentity]
        playWith event [energy, genius]
        chooseVillain
        offers isReflexes `shouldReturn` False
        exhaustedNow p `shouldReturn` True
        resolve [IdentityMessage p ReadiedIdentity]
        attackVillain p
        offers isReflexes `shouldReturn` False

    it "does not respond to another player's event" do
      tg <- testGame ["ghost-spider", "spider-man"]
      let p = player1 tg
          q = player2 tg
      inGame tg do
        asGhost p
        resolve [IdentityMessage q ChooseHeroForm]
        resolve [IdentityMessage p ExhaustedIdentity]
        event <- putInHand q "09021"
        runNow $ damageIdentity q GameRef (toDamage 2 FromAbility)
        playWith event []
        offers isReflexes `shouldReturn` False
        pass
        exhaustedNow p `shouldReturn` True

  describe "Gwen Stacy" do
    it "shuffles Ticket from discard into the deck once per round" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        [ticket] <- stackDiscard p ["27008"]
        choose "Gwen's action" (isGwenAction p)
        choose "shuffle Ticket" (isTargetChoice ticket)
        discardOf p >>= (`shouldSatisfy` notElem ticket)
        deckOf p >>= (`shouldSatisfy` elem ticket)
        _ <- stackDiscard p ["27008"]
        offers (isGwenAction p) `shouldReturn` False

    it "readies George instead of shuffling Ticket" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        george <- putIntoPlay p "27007"
        [ticket] <- stackDiscard p ["27008"]
        resolve [SupportMessage (supportIdOf george) ExhaustedSupport]
        choose "Gwen's action" (isGwenAction p)
        choose "ready George" (isTargetChoice $ supportIdOf george)
        fmap (map supportExhausted) (supportsWith "27007") `shouldReturn` [False]
        discardOf p >>= (`shouldSatisfy` elem ticket)
        runNow [EndRound]
        pass
        resolve [SupportMessage (supportIdOf george) ExhaustedSupport]
        offers (isGwenAction p) `shouldReturn` True

    it "has no action when Ticket is absent and George is ready" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "27007"
        offers (isGwenAction p) `shouldReturn` False

  describe "George Stacy" do
    it "attaches an event from hand, leaving it playable but not in the hand" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        george <- putIntoPlay p "27007"
        event <- putInHand p "09021"
        choose "attach an event" (isAbilityOfCard george)
        choose "store Warning" (isTargetChoice event)
        handOf p >>= (`shouldSatisfy` notElem event)
        getSupportHeldCards p `shouldReturn` [event]
        runNow $ damageIdentity p GameRef (toDamage 2 FromAbility)
        offers (isPlay event) `shouldReturn` True
        playWith event []
        pass
        getSupportHeldCards p `shouldReturn` []
        damageOf p `shouldReturn` 1

    it "does not attach a non-event and cannot exceed three cards" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        george <- putIntoPlay p "27007"
        _ <- putInHand p "01088"
        offers (isAbilityOfCard george) `shouldReturn` False
        for_ ["09021", "27002", "27004"] $ \code -> do
          event <- putInHand p code
          resolve [SupportMessage (supportIdOf george) ReadiedSupport]
          choose "attach an event" (isAbilityOfCard george)
          choose "store the event" (isTargetChoice event)
        resolve [SupportMessage (supportIdOf george) ReadiedSupport]
        _ <- putInHand p "09021"
        offers (isAbilityOfCard george) `shouldReturn` False
        length <$> getSupportHeldCards p `shouldReturn` 3

    it "discards its attached events when it leaves play" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        george <- putIntoPlay p "27007"
        event <- putInHand p "09021"
        choose "attach an event" (isAbilityOfCard george)
        choose "store Warning" (isTargetChoice event)
        resolve [RemoveFromPlay $ SupportRef $ supportIdOf george]
        discardOf p >>= (`shouldSatisfy` (\cards -> george `elem` cards && event `elem` cards))

  describe "Web-Bracelet" do
    it "draws after an interrupt and allows only one bracelet for that event" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        first <- putIntoPlay p "27009"
        second <- putIntoPlay p "27009"
        event <- putInHand p "09021"
        runNow $ damageIdentity p GameRef (toDamage 2 FromAbility)
        playWith event []
        before <- length <$> handOf p
        choose "first Web-Bracelet" (isAbilityOfCard first)
        length <$> handOf p `shouldReturn` (before + 1)
        offers (isAbilityOfCard second) `shouldReturn` False
        pass
        next <- putInHand p "09021"
        runNow $ damageIdentity p GameRef (toDamage 2 FromAbility)
        playWith next []
        offers (isAbilityOfCard first) `shouldReturn` False
        offers (isAbilityOfCard second) `shouldReturn` True
        choose "the other bracelet for another event" (isAbilityOfCard second)
        pass

    it "does not trigger for an action event or in alter ego" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        bracelet <- putIntoPlay p "27009"
        event <- putInHand p "09021"
        runNow $ damageIdentity p GameRef (toDamage 2 FromAbility)
        offers (isPlay event) `shouldReturn` False
        pass
        changeForm p
        action <- putInHand p "01005"
        energy <- putInHand p "01088"
        genius <- putInHand p "01089"
        playWith action [energy, genius]
        chooseVillain
        offers (isAbilityOfCard bracelet) `shouldReturn` False

  describe "Ticket to the Multiverse" do
    it "removes itself as cost, shuffles the discarded hand and discard pile, draws to hand size, and readies signature cards" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        ticket <- putIntoPlay p "27008"
        george <- putIntoPlay p "27007"
        bracelet <- putIntoPlay p "27009"
        other <- putIntoPlay p "01008"
        hand <- putInHand p "01088"
        [discarded] <- stackDiscard p ["01089"]
        resolve [IdentityMessage p ExhaustedIdentity, SupportMessage (supportIdOf george) ExhaustedSupport,
          UpgradeMessage (upgradeIdOf bracelet) ExhaustedUpgrade, UpgradeMessage (upgradeIdOf other) ExhaustedUpgrade]
        dealtBefore <- length <$> dealtTo p
        choose "use Ticket" (isAbilityOfCard ticket)
        pass
        upgradesWith "27008" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` notElem ticket)
        length <$> handOf p `shouldReturn` 5
        discardOf p `shouldReturn` []
        length <$> dealtTo p `shouldReturn` dealtBefore
        cards <- (<>) <$> handOf p <*> deckOf p
        cards `shouldSatisfy` (\cs -> hand `elem` cs && discarded `elem` cs)
        exhaustedNow p `shouldReturn` False
        fmap (map supportExhausted) (supportsWith "27007") `shouldReturn` [False]
        fmap (map upgradeExhausted) (upgradesWith "27009") `shouldReturn` [False]
        fmap (map upgradeExhausted) (upgradesWith "01008") `shouldReturn` [True]

    it "works in alter ego and draws to that form's hand size" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        ticket <- putIntoPlay p "27008"
        resolve [IdentityMessage p ExhaustedIdentity]
        choose "use Ticket as Gwen" (isAbilityOfCard ticket)
        length <$> handOf p `shouldReturn` 6
        exhaustedNow p `shouldReturn` False
        upgradesWith "27008" `shouldReturn` []
