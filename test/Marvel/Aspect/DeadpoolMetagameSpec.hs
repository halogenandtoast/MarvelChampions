module Marvel.Aspect.DeadpoolMetagameSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Villain.Types (villainConfused)
import TestImport

becomeHero :: TestGame -> GameAppT ()
becomeHero tg = changeForm (player1 tg) >> pass >> clearHand (player1 tg)

resourceCard :: Resource -> CardCode
resourceCard Energy = "01002"
resourceCard Mental = "01015"
resourceCard Physical = "01054"
resourceCard Wild = "44007"

slotAbility :: Int -> Choice -> Bool
slotAbility slot = isLabeledAbility (countLabel "metagameSlot" $ fromIntegral $ slot + 1)

paySlot :: IdentityId -> Int -> Resource -> GameAppT ()
paySlot you slot resource = do
  card <- putInHand you (resourceCard resource)
  choose "fill resource space" (slotAbility slot)
  choose "pay its matching resource" (isPayWith card)
  chooseIf isFinishPayment

fillDamageSlot :: IdentityId -> Int -> Resource -> GameAppT ()
fillDamageSlot you slot resource = do
  paySlot you slot resource
  choose "move damage from hero" (isTargetChoice you)
  pass

fillThreatSlot :: IdentityId -> Int -> Resource -> GameAppT ()
fillThreatSlot you slot resource = do
  paySlot you slot resource
  scheme <- mainSchemeIdNow
  choose "move threat from main scheme" (isTargetChoice scheme)
  pass

iconsOnHandCard :: IdentityId -> PlayerCard -> [Resource] -> GameAppT PlayerCard
iconsOnHandCard you original resources = do
  let card = original {pcCardDef = (pcCardDef original) {cdResources = [(PrintedResource, resource) | resource <- resources]}}
  withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs
    {playerIdentityHand = Hand [if pcCardId c == pcCardId card then card else c | c <- unHand $ playerIdentityHand attrs]}
  pure card

iconsOnDeckCard :: IdentityId -> PlayerCard -> [Resource] -> GameAppT PlayerCard
iconsOnDeckCard you original resources = do
  let card = original {pcCardDef = (pcCardDef original) {cdResources = [(PrintedResource, resource) | resource <- resources]}}
  withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs
    {playerIdentityDeck = Deck [if pcCardId c == pcCardId card then card else c | c <- unDeck $ playerIdentityDeck attrs]}
  pure card

playRps :: IdentityId -> PlayerCard -> PlayerCard -> GameAppT ()
playRps _ rps chosen = do
  choose "Rock, Paper, Scissors" (isAbilityOfCard rps)
  choose "choose the hand card" (isTargetChoice chosen)
  choose "discard the top card as cost" (isLabel $ countLabel "discardTopOfDeck" 1)
  pass

spec :: Spec
spec = describe "DeadpoolMetagame" do
  describe "Blackout" do
    it "moves threat into all six matching spaces, then discards itself and confuses the villain" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44053"
        placeMainThreat 6
        before <- mainSchemeThreatNow
        for_ (zip [0 ..] [Energy, Energy, Mental, Mental, Physical, Physical]) $ \(slot, resource) ->
          fillThreatSlot you slot resource
        chooseVillain
        pass
        mainSchemeThreatNow `shouldReturn` before - 6
        upgradesWith "44053" `shouldReturn` []
        villainConfused . toAttrs <$> getVillain `shouldReturn` True

    it "will not offer a wrong-resource space or let an occupied space be paid again" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44053"
        placeMainThreat 3
        _ <- putInHand you "01054"
        offers (slotAbility 0) `shouldReturn` False
        fillThreatSlot you 0 Energy
        _ <- putInHand you "01002"
        offers (slotAbility 0) `shouldReturn` False
        offers (slotAbility 1) `shouldReturn` True
        [attrs] <- upgradesWith "44053"
        Upgrade.upgradeOccupiedSlots attrs `shouldBe` [0]

    it "accepts a wild declared as the matching resource and never counts it as another type" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44053"
        placeMainThreat 2
        wild <- putInHand you "44007"
        choose "energy space" (slotAbility 0)
        choose "pay wild" (isPayWith wild)
        declareWildAs Energy
        chooseIf isFinishPayment
        main <- mainSchemeIdNow
        choose "main scheme" (isTargetChoice main)
        [attrs] <- upgradesWith "44053"
        Upgrade.upgradeOccupiedSlots attrs `shouldBe` [0]

    it "cannot be used without removable threat" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44053"
        _ <- putInHand you "01002"
        offers (slotAbility 0) `shouldReturn` False

  describe "Tic-Tac-Toe" do
    for_ [[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [2,4,6]] $ \line ->
      it ("wins printed line " <> show line <> " by moving damage and dealing all three stored tokens") do
        tg <- testGame ["captain-marvel"]
        inGame tg do
          becomeHero tg
          let you = player1 tg
              resource slot = [Energy, Mental, Physical] !! (slot `div` 3)
          _ <- putIntoPlay you "44057"
          setDamage you 6
          for_ line $ \slot -> fillDamageSlot you slot (resource slot)
          chooseVillain
          pass
          damageOf you `shouldReturn` 3
          villainDamageNow `shouldReturn` 3
          upgradesWith "44057" `shouldReturn` []

    it "deals every stored damage token, including tokens outside the winning line" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44057"
        setDamage you 6
        for_ [(0,Energy), (4,Mental), (2,Energy), (8,Physical)] $ \(slot, resource) -> fillDamageSlot you slot resource
        chooseVillain
        pass
        damageOf you `shouldReturn` 2
        villainDamageNow `shouldReturn` 4

    it "keeps separate positions and does not win with four tokens lacking a line" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44057"
        setDamage you 6
        for_ [(0,Energy), (1,Energy), (3,Mental), (4,Mental)] $ \(slot, resource) -> fillDamageSlot you slot resource
        [attrs] <- upgradesWith "44057"
        Upgrade.upgradeOccupiedSlots attrs `shouldMatchList` [0,1,3,4]
        villainDamageNow `shouldReturn` 0
        damageOf you `shouldReturn` 2

    it "requires matching payment and removes an occupied space from later choices" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44057"
        setDamage you 2
        _ <- putInHand you "01054"
        offers (slotAbility 0) `shouldReturn` False
        fillDamageSlot you 0 Energy
        _ <- putInHand you "01002"
        offers (slotAbility 0) `shouldReturn` False
        offers (slotAbility 1) `shouldReturn` True

    it "can heal an enemy character and cannot be used with no damaged character" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44057"
        payment <- putInHand you "01002"
        offers (slotAbility 0) `shouldReturn` False
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged GameRef (toDamage 2 FromAbility)]
        choose "fill first energy space" (slotAbility 0)
        choose "pay energy" (isPayWith payment)
        chooseIf isFinishPayment
        chooseVillain
        villainDamageNow `shouldReturn` 1

  describe "Rock, Paper, Scissors" do
    for_ [(ours, theirs) | ours <- [Energy, Mental, Physical, Wild], theirs <- [Energy, Mental, Physical, Wild]] $ \(ours, theirs) ->
      it (show ours <> " versus " <> show theirs <> " uses the printed dominance diagram, with ties losing") do
        tg <- testGame ["captain-marvel"]
        inGame tg do
          becomeHero tg
          let you = player1 tg
              wins = (ours, theirs) `elem` [(Energy,Mental), (Mental,Physical), (Physical,Energy), (Wild,Energy), (Wild,Mental), (Wild,Physical)]
          rps <- putIntoPlay you "44056"
          chosen <- putInHand you (resourceCard ours)
          [discarded] <- stackDeck you [resourceCard theirs]
          playRps you rps chosen
          hand <- handOf you
          (pcCardId discarded `elem` map pcCardId hand) `shouldBe` wins
          (pcCardId chosen `elem` map pcCardId hand) `shouldBe` True
          [attrs] <- upgradesWith "44056"
          Upgrade.upgradeExhausted attrs `shouldBe` True

    it "wins when any printed resource pair succeeds on multi-icon cards" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        rps <- putIntoPlay you "44056"
        original <- putInHand you "01002"
        chosen <- iconsOnHandCard you original [Energy, Physical]
        [top] <- stackDeck you ["01015"]
        discarded <- iconsOnDeckCard you top [Wild, Mental]
        playRps you rps chosen
        hand <- handOf you
        (pcCardId discarded `elem` map pcCardId hand) `shouldBe` True

    it "requires both a hand card and a deck card before paying its exhaust cost" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        rps <- putIntoPlay you "44056"
        offers (isAbilityOfCard rps) `shouldReturn` False
        original <- putInHand you "01002"
        _ <- iconsOnHandCard you original []
        offers (isAbilityOfCard rps) `shouldReturn` False
        _ <- putInHand you "01015"
        offers (isAbilityOfCard rps) `shouldReturn` True
        withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs {playerIdentityDeck = Deck []}
        offers (isAbilityOfCard rps) `shouldReturn` False

  describe "War" do
    it "counts printed star and boost icons as damage before using the discarded player card's cost" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        war <- putIntoPlay you "44058"
        _ <- setEncounterDeck [Minions.hiredGun] []
        [top] <- stackDeck you ["01054"]
        choose "War" (isAbilityOfCard war)
        chooseVillain
        pass
        damageOf you `shouldReturn` 3
        villainDamageNow `shouldReturn` 3
        hand <- handOf you
        (pcCardId top `elem` map pcCardId hand) `shouldBe` False
        [attrs] <- upgradesWith "44058"
        Upgrade.upgradeExhausted attrs `shouldBe` True

    it "treats a discarded resource card's absent cost as zero" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        war <- putIntoPlay you "44058"
        _ <- setEncounterDeck [Minions.weaponsRunner] []
        _ <- stackDeck you ["01088"]
        choose "War" (isAbilityOfCard war)
        chooseVillain
        pass
        damageOf you `shouldReturn` 1
        villainDamageNow `shouldReturn` 0

    it "finishes Deadpool's revival and change to alter ego before dealing the player-card damage" do
      tg <- testGame ["deadpool"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        war <- putIntoPlay you "44058"
        hp <- hitPointsOf you
        setDamage you (hp - 1)
        _ <- setEncounterDeck [Minions.hiredGun] []
        _ <- stackDeck you ["01054"]
        choose "War" (isAbilityOfCard war)
        choose "regenerate" (isLabeledAbility "$label.theRegeneratinDegenerate")
        chooseVillain
        pass
        damageOf you `shouldReturn` hp - 1
        playerIdentitySide <$> playerAttrs you `shouldReturn` B
        villainDamageNow `shouldReturn` 3
