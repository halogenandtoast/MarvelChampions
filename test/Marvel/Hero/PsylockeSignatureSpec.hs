module Marvel.Hero.PsylockeSignatureSpec (spec) where

import Marvel.Damage
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types (villainConfused)
import TestImport

settle :: GameAppT ()
settle = go (200 :: Int)
 where
  go 0 = error "Psylocke signature fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
      go (n-1)
    Nothing -> pure ()

advanceTo :: (Choice -> Bool) -> GameAppT ()
advanceTo wanted = go (100 :: Int) []
 where
  go 0 history = error $ "Psylocke signature choice not reached: " <> show history
  go n history = do
    reached <- offers wanted
    unless reached $ openQuestion >>= \case
      Just (you,q) -> do
        windows <- getCurrentWindows
        let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
        answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c])
        go (n-1) ((show (choicesOf q) <> " windows=" <> show windows) : history)
      Nothing -> error $ "Psylocke signature ended before choice: " <> show history

perform :: L.QueueT GameAppT () -> GameAppT ()
perform action = L.runQueueT (L.capture action) >>= resolve

inHeroWith :: IdentityId -> Int -> GameAppT ()
inHeroWith p katanas = do
  changeForm p
  pass
  clearHand p
  knives <- upgradesWith "41002a"
  for_ (take katanas knives) $ \u -> perform (L.flipPsiEnergy $ upgradeId u)

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code payments = do
  card <- putInHand p code
  payment <- traverse (putInHand p) payments
  playWith card payment
  pure card

prepareResponse :: IdentityId -> CardCode -> GameAppT (PlayerCard, PlayerCard)
prepareResponse p code = (,) <$> putInHand p code <*> putInHand p "01089"

respondWith :: (PlayerCard, PlayerCard) -> GameAppT ()
respondWith (card,payment) = do
  advanceTo (isPlay card)
  playWith card [payment]

spec :: Spec
spec = describe "Psylocke signatures" do
  describe "Flurry of Blades" do
    for_ [0..2] $ \katanas ->
      it ("resolves its damage and confusion with " <> show katanas <> " Katanas") do
        tg <- testGame ["psylocke"]
        let p = player1 tg
        inGame tg do
          inHeroWith p katanas
          _ <- playPaying p "41004" ["01088", "01089"]
          vid <- villainIdNow
          choose "initial damage" (isTargetChoice vid)
          for_ [1..2-katanas] $ \_ -> choose "knife confusion" (isTargetChoice vid)
          for_ [1..katanas] $ \_ -> choose "katana damage" (isTargetChoice vid)
          settle
          villainDamageNow `shouldReturn` fromIntegral (2 + 2*katanas)
          (villainConfused . toAttrs <$> getVillain) `shouldReturn` (katanas < 2)

    it "retaliates once for an enemy chosen repeatedly by the same attack" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        inHeroWith p 0
        mid <- addMinion p Minions.whiplash
        _ <- playPaying p "41004" ["01088", "01089"]
        choose "damage Whiplash" (isTargetChoice mid)
        choose "confuse Whiplash" (isTargetChoice mid)
        choose "choose Whiplash again" (isTargetChoice mid)
        settle
        minionDamageOf mid `shouldReturn` Just 2
        damageOf p `shouldReturn` 1

    it "also attacks and receives retaliation from a target chosen only for confusion" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        inHeroWith p 0
        mid <- addMinion p Minions.whiplash
        _ <- playPaying p "41004" ["01088", "01089"]
        vid <- villainIdNow
        choose "damage the villain" (isTargetChoice vid)
        choose "confuse Whiplash" (isTargetChoice mid)
        choose "confuse the villain" (isTargetChoice vid)
        settle
        minionDamageOf mid `shouldReturn` Just 0
        damageOf p `shouldReturn` 1

    it "is replaced by stun without dealing damage or confusing" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        inHeroWith p 0
        resolve [IdentityMessage p IdentityStunned]
        _ <- playPaying p "41004" ["01088", "01089"]
        settle
        villainDamageNow `shouldReturn` 0
        (villainConfused . toAttrs <$> getVillain) `shouldReturn` False
        isStunned p `shouldReturn` False

  describe "Mental Detection" do
    for_ [0..2] $ \katanas ->
      it ("removes one combined amount and draws with " <> show katanas <> " Katanas") do
        tg <- testGame ["psylocke"]
        let p = player1 tg
        inGame tg do
          inHeroWith p katanas
          placeMainThreat 5
          before <- mainSchemeThreatNow
          _ <- stackDeck p ["01088", "01090"]
          _ <- playPaying p "41005" ["01089"]
          mid <- mainSchemeIdNow
          choose "remove threat from the main scheme" (isTargetChoice mid)
          settle
          mainSchemeThreatNow `shouldReturn` before - fromIntegral (5 - 2*katanas)
          (length <$> handOf p) `shouldReturn` katanas

    it "is replaced by confusion, including its draws" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        inHeroWith p 2
        placeMainThreat 4
        before <- mainSchemeThreatNow
        resolve [IdentityMessage p IdentityConfused]
        _ <- playPaying p "41005" ["01089"]
        settle
        mainSchemeThreatNow `shouldReturn` before
        handOf p `shouldReturn` []
        isConfused p `shouldReturn` False

  describe "Psionic Redirect" do
    for_ [0..2] $ \katanas ->
      it ("prevents damage and confuses the attacker with " <> show katanas <> " Katanas") do
        tg <- testGame ["psylocke"]
        let p = player1 tg
        inGame tg do
          inHeroWith p katanas
          response <- prepareResponse p "41006"
          _ <- stackEncounterDeck [Treacheries.exhaustion]
          vid <- villainIdNow
          runNow [VillainMessage vid $ VillainAttacks p]
          respondWith response
          settle
          damageOf p `shouldReturn` fromIntegral (max 0 (2 - 2*katanas))
          (villainConfused . toAttrs <$> getVillain) `shouldReturn` (katanas < 2)

    it "cannot answer damage from a card ability" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        inHeroWith p 0
        (card,_) <- prepareResponse p "41006"
        L.runQueueT (L.capture $ L.damageIdentity p GameRef $ toDamage 3 FromAbility) >>= runNow
        offers (isPlay card) `shouldReturn` False
        settle
        damageOf p `shouldReturn` 3

  describe "Telepathic Suggestion" do
    for_ [0..2] $ \katanas ->
      it ("cancels the reveal and resolves weapon benefits with " <> show katanas <> " Katanas") do
        tg <- testGame ["psylocke"]
        let p = player1 tg
        inGame tg do
          inHeroWith p katanas
          placeMainThreat 6
          before <- mainSchemeThreatNow
          response <- prepareResponse p "41007"
          _ <- revealNow p Treacheries.advance
          respondWith response
          vid <- villainIdNow
          for_ [1..katanas] $ \_ -> choose "Katana damage" (isTargetChoice vid)
          mid <- mainSchemeIdNow
          for_ [1..2-katanas] $ \_ -> choose "Knife threat removal" (isTargetChoice mid)
          settle
          villainDamageNow `shouldReturn` fromIntegral (2*katanas)
          mainSchemeThreatNow `shouldReturn` before - fromIntegral (2-katanas)

    it "cancels a side scheme's reveal while retaining its entry and initial threat" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        inHeroWith p 2
        response <- prepareResponse p "41007"
        card <- revealNow p Schemes.interdimensionalPlunder
        respondWith response
        vid <- villainIdNow
        choose "first Katana damage" (isTargetChoice vid)
        choose "second Katana damage" (isTargetChoice vid)
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 2

    it "cancels surge as a When Revealed effect under Rules Reference v1.8" do
      tg <- testGame ["psylocke"]
      let p = player1 tg
      inGame tg do
        inHeroWith p 2
        response <- prepareResponse p "41007"
        _ <- setEncounterDeck [Treacheries.advance] []
        before <- length <$> dealtTo p
        _ <- revealNow p Treacheries.exhaustion
        respondWith response
        vid <- villainIdNow
        choose "first Katana damage" (isTargetChoice vid)
        choose "second Katana damage" (isTargetChoice vid)
        settle
        isExhaustedNow p `shouldReturn` False
        (length <$> dealtTo p) `shouldReturn` before

    it "cannot answer another player's revealed card" do
      tg <- testGame ["psylocke", "spider-man"]
      let p = player1 tg
      inGame tg do
        inHeroWith p 0
        (card,_) <- prepareResponse p "41007"
        _ <- revealNow (player2 tg) Treacheries.advance
        offers (isPlay card) `shouldReturn` False
