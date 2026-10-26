module Marvel.Aspect.RiseOfRedSkullPackSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGameFor)
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

-- | The ids of the cards a view shows as focused.
focusedIn :: Value -> [Value]
focusedIn = \case
  Object o | Just (Array cs) <- KeyMap.lookup "focusedCards" o -> [i | Object c <- toList cs, Just i <- [KeyMap.lookup "id" c]]
  _ -> []

spec :: Spec
spec = describe "The Rise of Red Skull" do
  describe "Team Training (04016)" do
    it "played under another player's control, goes to its owner's discard pile as it leaves play (RR Ownership and Control)" do
      tg <- testGame ["hawkeye", "spider-woman"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        training <- putInHand p "04016"
        payment <- traverse (putInHand p) ["01089"]
        playOn training other payment
        [s] <- supportsWith "04016"
        s.supportController `shouldBe` other
        resolve [RemoveFromPlay (SupportRef s.supportId)]
        map pcCardId <$> discardOf p >>= (`shouldSatisfy` elem (pcCardId training))
        map pcCardId <$> discardOf other >>= (`shouldSatisfy` notElem (pcCardId training))

  describe "Skilled Investigator (04047)" do
    it "after a side scheme is defeated, exhausts to draw 1 card" do
      tg <- testGame ["spider-woman"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        investigator <- putIntoPlay p "04047"
        sid <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        before <- sideSchemeThreat sid
        resolve [SideSchemeMessage sid (SideSchemeThwarted GameRef (before - 1))]
        choose "thwart" (isHeroThwart p)
        choose "Bomb Scare" (isTargetChoice (SchemeSideSchemeId sid))
        choose "Skilled Investigator" (isAbilityOfCard investigator)
        length <$> handOf p `shouldReturn` 1

  describe "Earth's Mightiest Heroes (04022)" do
    it "exhausts an Avenger character as its cost, then readies another" do
      tg <- testGame ["hawkeye"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        knight <- putIntoPlay p "04012"
        resolve [AllyMessage (allyIdOf knight) ExhaustedAlly]
        heroes <- putInHand p "04022"
        playWith heroes []
        -- Hawkeye is the one Avenger left ready, still picked to pay; the knight is the
        -- one to ready, still picked
        choose "Hawkeye pays" (isTargetChoice p)
        choose "ready the knight" (isTargetChoice (allyIdOf knight))
        exhaustedNow p `shouldReturn` True
        (.allyExhausted) <$> allyIn "04012" `shouldReturn` False

    it "cannot be played without an Avenger character to exhaust" do
      tg <- testGame ["hawkeye"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        resolve [IdentityMessage p ExhaustedIdentity]
        heroes <- putInHand p "04022"
        offers (isPlay heroes) `shouldReturn` False

  describe "Jessica Drew (04031b)" do
    it "looks at the top card of a deck, shown to her player alone (RR Look)" do
      tg <- testGame ["spider-woman", "hawkeye"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        [top] <- stackDeck other ["01088"]
        choose "Double Agent" (isNamedAbility "Double Agent")
        choose "Hawkeye's deck" (isTargetChoice other)
        g <- getGame
        mine <- liftIO $ publicGameFor (== p) g
        theirs <- liftIO $ publicGameFor (== other) g
        focusedIn mine `shouldBe` [toJSON (pcCardId top)]
        focusedIn theirs `shouldBe` []
        choose "done" (isLabel "$label.done")
        g' <- getGame
        focusedIn <$> liftIO (publicGameFor (== p) g') `shouldReturn` []

  describe "Marked for Death (04028)" do
    it "tucks the Clint Barton player's Mockingbird, found by his identity, and returns her when defeated" do
      tg <- testGame ["hawkeye"]
      let p = player1 tg
      inGame tg do
        clearHand p
        bird <- putInHand p "04004"
        scheme <- revealNow p SideSchemes.markedForDeath
        map pcCardId <$> handOf p >>= (`shouldSatisfy` notElem (pcCardId bird))
        resolve [SideSchemeMessage (sideSchemeOf scheme) DefeatSideScheme]
        map pcCardId <$> handOf p >>= (`shouldSatisfy` elem (pcCardId bird))
