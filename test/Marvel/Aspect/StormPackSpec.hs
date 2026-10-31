module Marvel.Aspect.StormPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Matchers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Phase
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  pass
  switchWeather p "36002"
  clearHand p

switchWeather :: IdentityId -> CardCode -> GameAppT ()
switchWeather p code = do
  current <- supportsWith code
  when (null current) do
    goddess <- putInHand p "36009"
    playWith goddess []
    choose "Weather" (\case CardLabel c _ -> c == code; _ -> False)
    pass

responseOf :: PlayerCard -> Choice -> Bool
responseOf c choice = isAbilityOfCard c choice && case choice of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

isVillainChoice :: Choice -> Bool
isVillainChoice = \case
  TargetLabel (VillainRef _) _ -> True
  TargetLabel (EnemyRef (EnemyVillainId _)) _ -> True
  _ -> False

attackWith :: PlayerCard -> GameAppT ()
attackWith c = allyIn (toCardCode c) >>= \a -> choose "ally attacks" (isAllyAttack a)

playPaid :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaid p code resources = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resources
  playWith card payment
  pure card

allyDamageNow :: PlayerCard -> GameAppT (Maybe Natural)
allyDamageNow c = getsGame (fmap (Ally.allyDamage . toAttrs) . HashMap.lookup (allyIdOf c) . gameAllies)

spec :: Spec
spec = describe "Storm pack aspect and basic cards" do
  describe "Havok" do
    it "adds printed boosts to this attack and its consequential damage" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "36014"
        _ <- stackEncounterDeck [Treacheries.explosion]
        attackWith c
        choose "villain" isVillainChoice
        choose "Havok forced interrupt" (responseOf c)
        pass
        villainDamageNow `shouldReturn` 4
        allyDamageNow c `shouldReturn` Nothing
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01111")

    it "counts a boost star as zero and clears the attack bonus before the next use" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "36014"
        _ <- stackEncounterDeck [Minions.weaponsRunner, Treacheries.explosion]
        attackWith c
        choose "villain" isVillainChoice
        choose "Havok forced interrupt" (responseOf c)
        pass
        villainDamageNow `shouldReturn` 2
        allyDamageNow c `shouldReturn` Just 1
        resolve [AllyMessage (allyIdOf c) ReadiedAlly]
        attackWith c
        choose "villain" isVillainChoice
        choose "Havok forced interrupt" (responseOf c)
        pass
        villainDamageNow `shouldReturn` 6
        allyDamageNow c `shouldReturn` Nothing

    it "does not discard an encounter card on a thwart" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putIntoPlay p "36014"
        _ <- stackEncounterDeck [Treacheries.explosion]
        before <- encounterDiscardCodes
        placeMainThreat 3
        a <- allyIn "36014"
        choose "thwart" (isAllyThwart a)
        chooseMainScheme
        pass
        encounterDiscardCodes `shouldReturn` before

  describe "Mirage" do
    it "stuns an enemy with SCH strictly below her current THW" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        switchWeather p "36005"
        c <- playPaid p "36015" ["01088", "01089"]
        choose "Mirage" (responseOf c)
        choose "villain" isVillainChoice
        pass
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` True

    it "does not offer enemies whose SCH equals her THW" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        switchWeather p "36005"
        mid <- addMinion p Minions.yonRogg
        c <- playPaid p "36015" ["01088", "01089"]
        choose "Mirage" (responseOf c)
        offers (isTargetChoice mid) `shouldReturn` False
        choose "villain" isVillainChoice
        pass

    it "does not offer a response when all enemies are stalwart" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        -- Clear Skies grants every character stalwart.
        asHero p
        c <- playPaid p "36015" ["01088", "01089"]
        offers (responseOf c) `shouldReturn` False

  describe "Gentle" do
    it "takes two consequential damage for attacking the villain" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "36016"
        attackWith c
        choose "villain" isVillainChoice
        pass
        villainDamageNow `shouldReturn` 3
        allyDamageNow c `shouldReturn` Just 2

    it "takes only one consequential damage for attacking a minion" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        c <- putIntoPlay p "36016"
        mid <- addMinion p Minions.yonRogg
        attackWith c
        choose "minion" (isTargetChoice mid)
        pass
        allyDamageNow c `shouldReturn` Just 1
        minionDamageOf mid `shouldReturn` Just 3

  describe "Pixie" do
    it "returns an X-Men ally when played from hand" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        forge <- putInDiscard p "36022"
        c <- playPaid p "36017" ["01088"]
        choose "Pixie" (responseOf c)
        choose "return Forge" (isTargetChoice forge)
        pass
        handOf p >>= (`shouldSatisfy` elem forge)
        discardOf p >>= (`shouldSatisfy` notElem forge)

    it "cannot return a non-X-Men ally" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putInDiscard p "01084"
        c <- playPaid p "36017" ["01088"]
        offers (responseOf c) `shouldReturn` False

    it "does not respond when put into play" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        forge <- putInDiscard p "36022"
        c <- putIntoPlay p "36017"
        offers (responseOf c) `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem forge)

  describe "Forge" do
    it "searches an X-Men support from the deck" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        [jet] <- stackDeck p ["36023"]
        c <- playPaid p "36022" ["01088"]
        choose "Forge" (responseOf c)
        choose "X-Jet" (isTargetChoice jet)
        pass
        handOf p >>= (`shouldSatisfy` elem jet)

    it "also searches the discard pile" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        mansion <- putInDiscard p "36025"
        c <- playPaid p "36022" ["01088"]
        choose "Forge" (responseOf c)
        choose "X-Mansion" (isTargetChoice mansion)
        pass
        handOf p >>= (`shouldSatisfy` elem mansion)

  describe "Uncanny X-Men" do
    it "adds one HP to controlled X-Men allies in alter-ego too" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putIntoPlay p "36018"
        _ <- putIntoPlay p "36016"
        a <- allyIn "36016"
        Ally.getModifiedHitPoints a `shouldReturn` 4

    it "discounts X-Men allies only while every controlled character is X-Men" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putIntoPlay p "36018"
        c <- putInHand p "36016"
        payment <- putInHand p "01088"
        playWith c [payment]
        pass
        allyIn "36016" >>= \a -> Ally.getModifiedHitPoints a `shouldReturn` 4

    it "does not discount allies in Mutant alter-ego form" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putIntoPlay p "36018"
        c <- putInHand p "36016"
        _ <- putInHand p "01088"
        offers (isPlay c) `shouldReturn` False

    it "loses the discount when a controlled character lacks X-Men" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putIntoPlay p "36018"
        _ <- putIntoPlay p "01002"
        clearHand p
        c <- putInHand p "36016"
        _ <- putInHand p "01088"
        offers (isPlay c) `shouldReturn` False

    it "does not grant HP to a non-X-Men ally" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putIntoPlay p "36018"
        _ <- putIntoPlay p "01002"
        a <- allyIn "01002"
        Ally.getModifiedHitPoints a `shouldReturn` 2

    it "cannot play a second Team under the same player" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putIntoPlay p "36018"
        second <- putInHand p "36018"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay second) `shouldReturn` False

  describe "Leadership Skill" do
    it "starts with three counters and adds one ATK for one ally attack" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        skill <- putIntoPlay p "36019"
        map (.upgradeUses) <$> upgradesWith "36019" `shouldReturn` [3]
        c <- putIntoPlay p "36016"
        attackWith c
        choose "villain" isVillainChoice
        choose "Leadership Skill" (responseOf skill)
        pass
        villainDamageNow `shouldReturn` 4
        map (.upgradeUses) <$> upgradesWith "36019" `shouldReturn` [2]
        a <- allyIn "36016"
        Ally.getModifiedAttack a `shouldReturn` 3

    it "adds one THW for an ally thwart and discards after the last use" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        skill <- putIntoPlay p "36019"
        c <- putIntoPlay p "36014"
        _ <- putIntoPlay p "36018"
        placeMainThreat 6
        before <- mainSchemeThreatNow
        for_ [1 :: Int .. 3] \_ -> do
          resolve [AllyMessage (allyIdOf c) ReadiedAlly]
          a <- allyIn "36014"
          choose "thwart" (isAllyThwart a)
          chooseMainScheme
          choose "Leadership Skill" (responseOf skill)
          pass
        mainSchemeThreatNow `shouldReturn` before - 6
        upgradesWith "36019" `shouldReturn` []

    it "cannot play a second copy" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putIntoPlay p "36019"
        c <- putInHand p "36019"
        _ <- putInHand p "01088"
        offers (isPlay c) `shouldReturn` False

  describe "To Me, My X-Men!" do
    it "puts an X-Men ally from the top five into play and returns it at phase end" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        [gentle] <- stackDeck p ["36016"]
        _ <- playPaid p "36020" ["01088"]
        choose "Gentle" (isTargetChoice gentle)
        pass
        allyDamageNow gentle `shouldReturn` Just 0
        start [EndPhase PlayerPhase]
        handOf p >>= (`shouldSatisfy` elem gentle)
        allyDamageNow gentle `shouldReturn` Nothing

    it "cannot be played without X-Men in the current form" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        clearHand p
        c <- putInHand p "36020"
        _ <- putInHand p "01088"
        offers (isPlay c) `shouldReturn` False

    it "does not find a sixth card or a non-X-Men ally" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        cards <- stackDeck p ["01084", "01088", "01089", "01090", "01088", "36016"]
        _ <- playPaid p "36020" ["01088"]
        offers (isTargetChoice $ last cards) `shouldReturn` False
        choose "no matching cards" (isLabel "$label.noMatchingCardsFound")
        pass
        getsGame (HashMap.size . gameAllies) `shouldReturn` 0

    it "does not return an ally that was defeated before phase end" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        [gentle] <- stackDeck p ["36016"]
        _ <- playPaid p "36020" ["01088"]
        choose "Gentle" (isTargetChoice gentle)
        pass
        resolve [RemoveFromPlay (AllyRef $ allyIdOf gentle)]
        start [EndPhase PlayerPhase]
        handOf p >>= (`shouldSatisfy` notElem gentle)
        discardOf p >>= (`shouldSatisfy` elem gentle)

    it "does not return a new instance of the same ally after it left play" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        [gentle] <- stackDeck p ["36016"]
        _ <- playPaid p "36020" ["01088"]
        choose "Gentle" (isTargetChoice gentle)
        pass
        resolve [ReturnToHand (AllyRef $ allyIdOf gentle)]
        playWith gentle =<< traverse (putInHand p) ["01088", "01089"]
        pass
        start [EndPhase PlayerPhase]
        allyDamageNow gentle `shouldReturn` Just 0
        handOf p >>= (`shouldSatisfy` notElem gentle)

    it "returns the same ally even if Possessed changed it into a minion" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        [gentle] <- stackDeck p ["36016"]
        _ <- playPaid p "36020" ["01088"]
        choose "Gentle" (isTargetChoice gentle)
        pass
        _ <- revealNow p Attachments.possessed
        pass
        allyDamageNow gentle `shouldReturn` Nothing
        start [EndPhase PlayerPhase]
        handOf p >>= (`shouldSatisfy` elem gentle)
        getsGame (HashMap.member (MinionId $ unAllyId $ allyIdOf gentle) . gameMinions) `shouldReturn` False

  describe "Hangar Bay" do
    it "readies an ally after it survives defending and exhausts the support" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        bay <- putIntoPlay p "36035"
        c <- putIntoPlay p "36016"
        vid <- villainIdNow
        start [VillainMessage vid (VillainAttacksWithoutBoost p)]
        choose "Gentle defends" (\case AllyDefend aid _ -> aid == allyIdOf c; _ -> False)
        choose "Hangar Bay" (responseOf bay)
        pass
        Ally.allyExhausted <$> allyIn "36016" `shouldReturn` False
        map (.supportExhausted) <$> supportsWith "36035" `shouldReturn` [True]

    it "does not trigger if the defending ally is defeated" do
      tg <- testGame ["storm"]
      inGame tg do
        let p = player1 tg
        asHero p
        bay <- putIntoPlay p "36035"
        c <- putIntoPlay p "36017"
        vid <- villainIdNow
        start [VillainMessage vid (VillainAttacksWithoutBoost p)]
        choose "Pixie defends" (\case AllyDefend aid _ -> aid == allyIdOf c; _ -> False)
        offers (responseOf bay) `shouldReturn` False
        pass
        allyDamageNow c `shouldReturn` Nothing
        map (.supportExhausted) <$> supportsWith "36035" `shouldReturn` [False]
