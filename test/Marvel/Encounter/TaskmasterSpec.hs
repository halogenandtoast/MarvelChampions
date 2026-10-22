module Marvel.Encounter.TaskmasterSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Data.Text qualified as T
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.SideScheme.Types (Attrs (sideSchemeHeldCards))
import Marvel.Trait (Trait (Captive))
import TestImport

taskmaster :: [Text] -> IO TestGame
taskmaster = testGameAgainst "04093"

setAsideCaptives :: GameAppT [PlayerCard]
setAsideCaptives = filter (cardMatch (CardWithTrait Captive)) <$> getSetAsidePlayerCards

heldUnder :: SideSchemeId -> GameAppT [PlayerCard]
heldUnder sid = getsGame (maybe [] (sideSchemeHeldCards . toAttrs) . HashMap.lookup sid . gameSideSchemes)

spec :: Spec
spec = describe "Taskmaster" do
  describe "Hunting Down Heroes" do
    it "sets the four Captive allies aside out of play, nobody's cards" do
      tg <- taskmaster ["spider-man"]
      inGame tg do
        captives <- setAsideCaptives
        sort (map (toCardCode . pcCardDef) captives) `shouldBe` ["04097", "04098", "04099", "04100" :: CardCode]
        map pcOwner captives `shouldBe` replicate 4 Nothing

  describe "Captured by Hydra" do
    it "holds a random Captive ally; the player who defeats it takes the ally into hand, as theirs, and removes the scheme from the game" do
      tg <- taskmaster ["spider-man", "she-hulk"]
      inGame tg do
        [p1, p2] <- players
        captured <- revealNow p1 SideSchemes.capturedByHydra
        pass
        let sid = sideSchemeOf captured
        [ally] <- heldUnder sid
        cardMatch (CardWithTrait Captive) ally `shouldBe` True
        length <$> setAsideCaptives `shouldReturn` 3
        -- 5 threat, not per player
        sideSchemeThreat sid `shouldReturn` 5
        -- the second player thwarts it to nothing
        runNow [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p2) 5)]
        pass
        getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False
        hand2 <- handOf p2
        [rescued] <- pure [c | c <- hand2, pcCardId c == pcCardId ally]
        pcOwner rescued `shouldBe` Just p2
        pcController rescued `shouldBe` Just p2
        told <- liftIO $ T.concat . map entryText <$> readIORef tg.logged
        told `shouldSatisfy` T.isInfixOf "$log.takesControlOf"
        map pcCardId <$> handOf p1 >>= (`shouldSatisfy` notElem (pcCardId ally))
        -- removed from the game: not in the encounter discard pile
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "04107")
        -- the rescued ally is their card: discarded, it goes to their discard pile
        resolve [DiscardedCard (PlayerCard rescued)]
        map pcCardId <$> discardOf p2 >>= (`shouldSatisfy` elem (pcCardId ally))

    it "holds nothing once every Captive ally is taken" do
      tg <- taskmaster ["spider-man"]
      inGame tg do
        [p] <- players
        sids <- for [1 .. 5 :: Int] \_ -> do
          c <- revealNow p SideSchemes.capturedByHydra
          pass
          pure (sideSchemeOf c)
        held <- traverse heldUnder sids
        map length held `shouldBe` [1, 1, 1, 1, 0]

  describe "Photographic Reflexes" do
    it "turns a player's attack on Taskmaster back on them, then is discarded" do
      tg <- taskmaster ["spider-man"]
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p (ChangedToForm A)]
        _ <- revealNow p Attachments.photographicReflexes
        pass
        before <- villainDamageNow
        damage <- damageOf p
        vid <- villainIdNow
        choose "attack" (isHeroAttack p)
        chooseIf (isTargetChoice vid)
        choose "the forced interrupt" isAnyAbility
        pass
        villainDamageNow `shouldReturn` before
        -- Spider-Man's ATK is 2
        damageOf p `shouldReturn` (damage + 2)
        encounterDiscardCodes >>= (`shouldSatisfy` elem "04104")
