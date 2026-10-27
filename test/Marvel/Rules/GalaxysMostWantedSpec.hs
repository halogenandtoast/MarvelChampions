-- | The rules The Galaxy's Most Wanted brings (rulebook p.3, RR v1.8): victory X and the
-- victory display, patrol, stalwart, peril and the amplify icon.
module Marvel.Rules.GalaxysMostWantedSpec (spec) where

import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Ability (isForcedAbility)
import Marvel.Game.Public (publicGame)
import Marvel.Keyword (Keyword (Stalwart))
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Minion.Types (overMinion)
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Treachery.Cards qualified as TreacheryCards
import TestImport

-- | The minion gains stalwart, as an attachment would give it.
gainStalwart :: MinionId -> GameAppT ()
gainStalwart m = withGame_ $ entitiesL . minionsL . ix m %~ overMinion \a ->
  a {minionCardDef = (minionCardDef a) {cdKeywords = HashSet.insert Stalwart (cdKeywords (minionCardDef a))}}

minionStatus :: MinionId -> GameAppT (Bool, Bool)
minionStatus m = getsGame $ maybe (False, False) ((\a -> (a.minionStunned, a.minionConfused)) . toAttrs) . HashMap.lookup m . gameMinions

spec :: Spec
spec = describe "The Galaxy's Most Wanted rules" do
  describe "victory X and the victory display" do
    it "a side scheme added to the victory display leaves play without being discarded" do
      tg <- testGame ["spider-man"]
      let you = player1 tg
      inGame tg do
        card <- revealNow you SideSchemeCards.fugitiveRecovery
        resolve [AddToVictoryDisplay (SideSchemeRef (sideSchemeOf card))]
        inPlay <- getsGame (HashMap.member (sideSchemeOf card) . gameSideSchemes)
        display <- getVictoryDisplay
        discard <- encounterDiscardNow
        liftIO $ (inPlay, display, card `elem` discard) `shouldBe` (False, [card], False)

    it "a defeated card without victory X goes to its discard pile" do
      tg <- testGame ["spider-man"]
      let you = player1 tg
      inGame tg do
        m <- addMinion you MinionCards.hydraMercenary
        resolve [MinionMessage m (MinionDamaged (toSource you) (toDamage 9 FromAbility))]
        display <- getVictoryDisplay
        discard <- map toCardCode <$> encounterDiscardNow
        liftIO $ (null display, toCardCode MinionCards.hydraMercenary `elem` discard) `shouldBe` (True, True)

    it "the board shows the victory display" do
      tg <- testGame ["spider-man"]
      let you = player1 tg
      inGame tg do
        m <- addMinion you MinionCards.badoonHeadhunter
        resolve [MinionMessage m (MinionDamaged (toSource you) (toDamage 7 FromAbility))]
        view <- liftIO . publicGame =<< getGame
        let shown = [c | Object o <- [view], Just (Array cs) <- [KeyMap.lookup "victoryDisplay" o], c <- toList cs]
        liftIO $ length shown `shouldBe` 1

    it "an older saved game without a victory display loads with an empty one" do
      tg <- testGame ["spider-man"]
      game <- inGame tg getGame
      let legacy = case toJSON game of
            Object o -> Object (KeyMap.delete "victoryDisplay" (KeyMap.delete "peril" o))
            value -> value
      fmap gameVictoryDisplay (fromJSON' legacy) `shouldBe` Just []

  describe "hinder X per player" do
    it "counts the players who started the scenario, one eliminated among them" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        resolve [IdentityMessage (player2 tg) IdentityDefeated]
        card <- revealNow (player1 tg) SideSchemeCards.fugitiveRecovery
        threat <- sideSchemeThreat (sideSchemeOf card)
        liftIO $ threat `shouldBe` 8

  describe "patrol" do
    it "a thwart-labeled card cannot thwart the main scheme while a patrol minion is engaged" do
      tg <- testGame ["spider-man"]
      let you = player1 tg
      inGame tg do
        placeMainThreat 3
        changeForm you
        _ <- addMinion you MinionCards.headhuntersHenchman
        card <- revealNow you SideSchemeCards.fugitiveRecovery
        clearHand you
        forJustice <- putInHand you "01060"
        a <- putInHand you "01090"
        b <- putInHand you "01090"
        playWith forJustice [a, b]
        m <- mainSchemeIdNow
        mainChoice <- offers (isTargetChoice m)
        sideChoice <- offers (isTargetChoice (sideSchemeOf card))
        liftIO $ (mainChoice, sideChoice) `shouldBe` (False, True)

    it "removing threat from the main scheme otherwise than by a thwart is not kept from them" do
      tg <- testGame ["spider-man"]
      let you = player1 tg
      inGame tg do
        placeMainThreat 3
        _ <- addMinion you MinionCards.headhuntersHenchman
        m <- mainSchemeIdNow
        let removal = TargetLabel (MainSchemeRef m) [Run [MainSchemeMessage m (MainSchemeThreatRemoved (toSource you) 1)]]
            thwart = TargetLabel (MainSchemeRef m) [Run [MainSchemeMessage m (MainSchemeThwarted (toSource you) 1)]]
        runNow [Ask you (ChooseOne [removal, thwart, Label "$label.done" []])]
        choices <- maybe [] snd <$> asked
        liftIO $ length choices `shouldBe` 2

  describe "stalwart" do
    it "a stalwart minion cannot be stunned or confused" do
      tg <- testGame ["spider-man"]
      let you = player1 tg
      inGame tg do
        m <- addMinion you MinionCards.hydraMercenary
        gainStalwart m
        resolve [MinionMessage m (MinionStunned (toSource you)), GiveStatus (MinionRef m) ConfusedStatus]
        status <- minionStatus m
        liftIO $ status `shouldBe` (False, False)

    it "a minion gaining stalwart while stunned and confused has those status cards removed (FAQ)" do
      tg <- testGame ["spider-man"]
      let you = player1 tg
      inGame tg do
        m <- addMinion you MinionCards.hydraMercenary
        resolve [MinionMessage m (MinionStunned (toSource you)), MinionMessage m (MinionConfused (toSource you))]
        before <- minionStatus m
        gainStalwart m
        resolve [ClearBoosts]
        after <- minionStatus m
        liftIO $ (before, after) `shouldBe` ((True, True), (False, False))

  describe "peril" do
    it "while a player resolves a peril card, the others trigger nothing but what is forced" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        open <- filter (not . isForcedAbility) <$> getAbilitiesFor (player1 tg)
        card <- encounterCard TreacheryCards.blindSide
        runNow [RevealEncounterCard (player2 tg) card]
        during <- filter (not . isForcedAbility) <$> getAbilitiesFor (player1 tg)
        own <- getAbilitiesFor (player2 tg)
        liftIO do
          open `shouldSatisfy` notNull
          during `shouldBe` []
          own `shouldSatisfy` notNull
 where
  fromJSON' v = case fromJSON v of
    Success g -> Just g
    _ -> Nothing
