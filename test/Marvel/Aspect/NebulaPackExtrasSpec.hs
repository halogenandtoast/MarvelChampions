module Marvel.Aspect.NebulaPackExtrasSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Query
import Marvel.Trait
import TestImport hiding (isAllyAttack)

inHero :: IdentityId -> GameAppT ()
inHero p = changeForm p >> pass >> clearHand p

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code resources = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resources
  playWith card payment
  pure card

playAttached :: IdentityId -> CardCode -> AllyId -> [CardCode] -> GameAppT PlayerCard
playAttached p code aid resources = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resources
  playOn card aid payment
  pure card

isCardAbility :: PlayerCard -> Choice -> Bool
isCardAbility card choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

isAllyAttack :: AllyId -> Choice -> Bool
isAllyAttack aid = \case
  UseAbility ab -> AllyAttack aid `elem` abilityChoices ab
  _ -> False

allyAttrs :: AllyId -> GameAppT (Attrs Ally)
allyAttrs aid = getsGame (maybe (error "missing ally") toAttrs . HashMap.lookup aid . gameAllies)

upgradeAttrs :: PlayerCard -> GameAppT (Attrs Upgrade)
upgradeAttrs card = getsGame (maybe (error "missing upgrade") toAttrs . HashMap.lookup (upgradeIdOf card) . gameUpgrades)

trainingAttrs :: PlayerCard -> GameAppT (Attrs Support)
trainingAttrs card = getsGame (maybe (error "missing support") toAttrs . HashMap.lookup (supportIdOf card) . gameSupports)

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . playerIdentityDeck <$> playerAttrs p

spec :: Spec
spec = describe "Nebula pack extras" do
  describe "Daughters of Thanos" do
    it "draws three for Nebula when the Gamora ally is in play" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "22002"
        clearHand p
        _ <- playPaying p "22022" ["01088"]
        length <$> handOf p `shouldReturn` 3

    it "can also be played by Gamora with the Nebula ally" do
      tg <- testGame ["gamora"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "18002"
        clearHand p
        _ <- playPaying p "22022" ["01088"]
        length <$> handOf p `shouldReturn` 3

    it "accepts the named partner's hero form in multiplayer" do
      tg <- testGame ["nebula", "gamora"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        inHero p
        runNow [IdentityMessage other $ ChangedToForm A]
        clearHand p
        _ <- playPaying p "22022" ["01088"]
        length <$> handOf p `shouldReturn` 3

    it "does not count a same-named partner in alter-ego form" do
      tg <- testGame ["nebula", "gamora"]
      let p = player1 tg
      inGame tg do
        inHero p
        card <- putInHand p "22022"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

    it "requires the partner and can only be played as a hero action" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        card <- putInHand p "22022"
        _ <- putInHand p "01088"
        _ <- putIntoPlay p "22002"
        offers (isPlay card) `shouldReturn` False
        inHero p
        missing <- putInHand p "22022"
        _ <- putInHand p "01088"
        gamoras <- selectList (AllyWithTitle "Gamora")
        for_ gamoras $ \aid -> resolve [RemoveFromPlay $ AllyRef aid]
        offers (isPlay missing) `shouldReturn` False

    it "includes only the printed one-copy limit in Nebula's starter deck" do
      let deck = starter "nebula"
      (sum [n | (code, n) <- deck.cards, code == "22022"] `shouldBe` (1 :: Int)) :: IO ()

  describe "Energy Spear" do
    it "grants two ATK and piercing to its attached Guardian ally" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        gamora <- putIntoPlay p "22002"
        _ <- playAttached p "22032" (allyIdOf gamora) ["01088"]
        attrs <- allyAttrs (allyIdOf gamora)
        Ally.getModifiedAttack attrs `shouldReturn` 4
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) ToughStatus]
        choose "Gamora attacks" (isAllyAttack $ allyIdOf gamora)
        chooseVillain
        villainDamageNow `shouldReturn` 4

    it "does not grant piercing to your identity's own attacks" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        gamora <- putIntoPlay p "22002"
        _ <- playAttached p "22032" (allyIdOf gamora) ["01088"]
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) ToughStatus]
        choose "Nebula attacks" (isHeroAttack p)
        chooseVillain
        villainDamageNow `shouldReturn` 0

    it "can only attach to a Guardian ally and allows at most one per ally" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        gamora <- putIntoPlay p "22002"
        cosmo <- putIntoPlay p "17020"
        tigra <- putIntoPlay p "01051"
        _ <- playAttached p "22032" (allyIdOf gamora) ["01088"]
        second <- putInHand p "22032"
        resource <- putInHand p "01088"
        choose "play another Spear" (isPlay second)
        offers (isTargetChoice $ allyIdOf gamora) `shouldReturn` False
        offers (isTargetChoice $ allyIdOf tigra) `shouldReturn` False
        offers (isTargetChoice p) `shouldReturn` False
        choose "the other Guardian ally" (isTargetChoice $ allyIdOf cosmo)
        payWith [resource]

    it "recognizes the Guardian trait granted by Honorary Guardian" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        tigra <- putIntoPlay p "01051"
        _ <- playAttached p "22035" (allyIdOf tigra) []
        _ <- playAttached p "22032" (allyIdOf tigra) ["01088"]
        attrs <- allyAttrs (allyIdOf tigra)
        Ally.getModifiedAttack attrs `shouldReturn` 4

    it "does not require its player to have Guardian and follows another player's ally control" do
      tg <- testGame ["spider-man", "nebula"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        cosmo <- putIntoPlay other "17020"
        spear <- playAttached p "22032" (allyIdOf cosmo) ["01088"]
        attrs <- upgradeAttrs spear
        liftIO $ (upgradeController attrs, upgradeOwner attrs) `shouldBe` (other, p)
        ally <- allyAttrs (allyIdOf cosmo)
        Ally.getModifiedAttack ally `shouldReturn` 3

  describe "Guardians of the Galaxy" do
    it "draws one after you play an upgrade on an ally when all your characters are Guardians" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        team <- putIntoPlay p "22033"
        gamora <- putIntoPlay p "22002"
        clearHand p
        _ <- playAttached p "22032" (allyIdOf gamora) ["01088"]
        choose "Guardians response" (isCardAbility team)
        length <$> handOf p `shouldReturn` 1

    it "has no exhaustion or round limit on its granted response" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        team <- putIntoPlay p "22033"
        gamora <- putIntoPlay p "22002"
        clearHand p
        _ <- playAttached p "22032" (allyIdOf gamora) ["01088"]
        choose "first response" (isCardAbility team)
        _ <- playAttached p "22035" (allyIdOf gamora) []
        choose "second response this round" (isCardAbility team)
        length <$> handOf p `shouldReturn` 2
        supportExhausted <$> trainingAttrs team `shouldReturn` False

    it "does not gain the response while any controlled ally is not a Guardian" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        team <- putIntoPlay p "22033"
        gamora <- putIntoPlay p "22002"
        _ <- putIntoPlay p "01051"
        _ <- playAttached p "22032" (allyIdOf gamora) ["01088"]
        offers (isCardAbility team) `shouldReturn` False

    it "can gain its response from the very Honorary Guardian upgrade just played" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        team <- putIntoPlay p "22033"
        tigra <- putIntoPlay p "01051"
        clearHand p
        _ <- playAttached p "22035" (allyIdOf tigra) []
        choose "all characters now are Guardians" (isCardAbility team)
        length <$> handOf p `shouldReturn` 1

    it "checks your identity as well as your allies" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        team <- putIntoPlay p "22033"
        gamora <- putIntoPlay p "22002"
        clearHand p
        _ <- playAttached p "22032" (allyIdOf gamora) ["01088"]
        offers (isCardAbility team) `shouldReturn` False

    it "can respond in alter-ego form when Honorary Guardian grants that identity Guardian" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        team <- putIntoPlay p "22033"
        honorary <- putInHand p "22035"
        playOn honorary p []
        gamora <- putIntoPlay p "22002"
        runNow [IdentityMessage p $ ChangedToForm B]
        clearHand p
        _ <- playAttached p "22032" (allyIdOf gamora) ["01088"]
        choose "Guardian alter ego responds" (isCardAbility team)
        length <$> handOf p `shouldReturn` 1

    it "does not respond to upgrades on identities or merely put-into-play upgrades" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        team <- putIntoPlay p "22033"
        gamora <- putIntoPlay p "22002"
        _ <- playPaying p "01065" ["01088"]
        offers (isCardAbility team) `shouldReturn` False
        card <- putInHand p "22032"
        runNow [PutCardIntoPlay p card (TargetPayment $ AllyRef $ allyIdOf gamora) Nothing False]
        offers (isCardAbility team) `shouldReturn` False

    it "allows the upgrade to be played on another player's ally" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        inHero p
        team <- putIntoPlay p "22033"
        cosmo <- putIntoPlay other "17020"
        clearHand p
        _ <- playAttached p "22032" (allyIdOf cosmo) ["01088"]
        choose "the payer draws" (isCardAbility team)
        length <$> handOf p `shouldReturn` 1

    it "can be played under another player's control and responds to that player's plays" do
      tg <- testGame ["nebula", "gamora"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        inHero p
        runNow [IdentityMessage other $ ChangedToForm A]
        cosmo <- putIntoPlay other "17020"
        team <- putInHand p "22033"
        resource <- putInHand p "01088"
        playOn team other [resource]
        attrs <- trainingAttrs team
        liftIO $ supportController attrs `shouldBe` other
        _ <- playAttached p "22032" (allyIdOf cosmo) ["01088"]
        offers (isCardAbility team) `shouldReturn` False
        endTurn
        clearHand other
        _ <- playAttached other "12018" (allyIdOf cosmo) ["01088"]
        choose "new controller responds" (isCardAbility team)
        length <$> handOf other `shouldReturn` 1

    it "enforces one Team card per player across different support names" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "21015"
        team <- putInHand p "22033"
        _ <- putInHand p "01088"
        offers (isPlay team) `shouldReturn` False

  describe "Defensive Training" do
    it "enters with two training counters, exhausts, and shuffles a Protection event from your discard" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        training <- putIntoPlay p "22034"
        supportUses <$> trainingAttrs training `shouldReturn` 2
        [green, red, upgrade] <- stackDiscard p ["01077", "01052", "01081"]
        choose "Defensive Training" (isCardAbility training)
        offers (isTargetChoice red) `shouldReturn` False
        offers (isTargetChoice upgrade) `shouldReturn` False
        choose "the Protection event" (isTargetChoice green)
        attrs <- trainingAttrs training
        liftIO $ (supportUses attrs, supportExhausted attrs) `shouldBe` (1, True)
        deck <- deckOf p
        discard <- discardOf p
        liftIO $ green `elem` deck `shouldBe` True
        liftIO $ green `elem` discard `shouldBe` False
        offers (isCardAbility training) `shouldReturn` False

    it "discards after spending the second training counter but completes that shuffle" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        training <- putIntoPlay p "22034"
        [first, second] <- stackDiscard p ["01077", "01078"]
        choose "first training" (isCardAbility training)
        choose "first event" (isTargetChoice first)
        resolve [SupportMessage (supportIdOf training) ReadiedSupport]
        choose "second training" (isCardAbility training)
        choose "second event" (isTargetChoice second)
        supportsWith "22034" `shouldReturn` []
        discard <- discardOf p
        deck <- deckOf p
        liftIO $ all (`elem` deck) [first, second] `shouldBe` True
        liftIO $ training `elem` discard `shouldBe` True

    it "does not spend a counter when there is no eligible discarded event" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        training <- putIntoPlay p "22034"
        _ <- stackDiscard p ["01075", "01081", "01052"]
        offers (isCardAbility training) `shouldReturn` False
        supportUses <$> trainingAttrs training `shouldReturn` 2
        supportExhausted <$> trainingAttrs training `shouldReturn` False

    it "cannot use another player's discard pile" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        training <- putIntoPlay p "22034"
        _ <- stackDiscard other ["01077"]
        offers (isCardAbility training) `shouldReturn` False

    it "is an alter-ego action and permits two independent copies" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        first <- putIntoPlay p "22034"
        second <- putIntoPlay p "22034"
        _ <- stackDiscard p ["01077", "01078"]
        inHero p
        offers (isCardAbility first) `shouldReturn` False
        offers (isCardAbility second) `shouldReturn` False
        runNow [IdentityMessage p $ ChangedToForm B]
        offers (isCardAbility first) `shouldReturn` True
        offers (isCardAbility second) `shouldReturn` True

  describe "Honorary Guardian" do
    it "requires Guardian on the playing identity" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        card <- putInHand p "22035"
        offers (isPlay card) `shouldReturn` False
        changeForm p
        offers (isPlay card) `shouldReturn` True

    it "grants one hit point and Guardian to a friendly ally" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        tigra <- putIntoPlay p "01051"
        _ <- playAttached p "22035" (allyIdOf tigra) []
        attrs <- allyAttrs (allyIdOf tigra)
        Ally.getModifiedHitPoints attrs `shouldReturn` 4
        selectAny (AllyWithId (allyIdOf tigra) <> AllyWithTrait Guardian) `shouldReturn` True

    it "grants its hit point and trait to an identity in both forms" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        card <- putInHand p "22035"
        playOn card p []
        hitPointsOf p `shouldReturn` 10
        runNow [IdentityMessage p $ ChangedToForm B]
        hitPointsOf p `shouldReturn` 10
        selectAny (IdentityWithId p <> IdentityWithTrait Guardian) `shouldReturn` True

    it "attaches to another player's character, following control while retaining ownership" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        inHero p
        tigra <- putIntoPlay other "01051"
        honorary <- playAttached p "22035" (allyIdOf tigra) []
        attrs <- upgradeAttrs honorary
        liftIO $ (upgradeController attrs, upgradeOwner attrs) `shouldBe` (other, p)
        resolve [AllyMessage (allyIdOf tigra) $ AllyChangedController p]
        updated <- upgradeAttrs honorary
        liftIO $ (upgradeController updated, upgradeOwner updated) `shouldBe` (p, p)

    it "can target friendly characters but never an enemy" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        tigra <- putIntoPlay p "01051"
        enemy <- addMinion p Minions.hydraMercenary
        card <- putInHand p "22035"
        choose "play Honorary Guardian" (isPlay card)
        offers (isTargetChoice enemy) `shouldReturn` False
        offers (isTargetChoice p) `shouldReturn` True
        choose "the friendly ally" (isTargetChoice $ allyIdOf tigra)
        selectAny (AllyWithId (allyIdOf tigra) <> AllyWithTrait Guardian) `shouldReturn` True

    it "limits each friendly character to one copy without forbidding another character" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        tigra <- putIntoPlay p "01051"
        _ <- playAttached p "22035" (allyIdOf tigra) []
        second <- putInHand p "22035"
        choose "play the second copy" (isPlay second)
        offers (isTargetChoice $ allyIdOf tigra) `shouldReturn` False
        -- The sole eligible identity is selected automatically.
        hitPointsOf p `shouldReturn` 10

    it "keeps character-targeted bonuses when the attached ally becomes a hostile minion" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        tigra <- putIntoPlay p "01051"
        honorary <- playAttached p "22035" (allyIdOf tigra) []
        let mid = MinionId $ unAllyId $ allyIdOf tigra
        resolve [AllyBecomesMinion $ allyIdOf tigra]
        transformed <- getsGame (maybe (error "missing minion") toAttrs . HashMap.lookup mid . gameMinions)
        Minion.getModifiedHitPoints transformed `shouldReturn` 4
        selectAny (MinionWithId mid <> MinionWithTrait Guardian) `shouldReturn` True
        selectAny (UpgradeWithId $ upgradeIdOf honorary) `shouldReturn` True
