module Marvel.Aspect.FalconPackSpec (spec) where

import TestImport
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Keyword
import Marvel.Trait qualified as T

asFalcon :: IdentityId -> GameAppT ()
asFalcon p = changeForm p >> pass >> clearHand p
abilityOn :: IsRef a => a -> Natural -> Choice -> Bool
abilityOn target n = \case
  UseAbility ab -> abilitySource ab == toSource target && abilityIndex ab == n
  _ -> False

spec :: Spec
spec = describe "Falcon pack" do
  it "Aero gives aerial characters of the chosen player one attack" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      aero <- putIntoPlay p "53015"
      choose "Aero" (abilityOn (allyIdOf aero) 1)
      choose "player" (isTargetChoice p)
      pass
      getModifiers p >>= (`shouldSatisfy` elem (AttackModifier 1))
      getModifiers (allyIdOf aero) >>= (`shouldSatisfy` elem (AttackModifier 1))

  it "Flight Squadron raises ally limit only when every ally is Aerial" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      _ <- putIntoPlay p "53020"
      getModifiers p >>= (`shouldSatisfy` elem (AllyLimitModifier 1))
      _ <- putIntoPlay p "01083"
      getModifiers p >>= (`shouldSatisfy` notElem (AllyLimitModifier 1))

  it "Captain America finds his unowned set-aside Shield and adds it to hand" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      captain <- putIntoPlay p "53023"
      strength <- putInHand p "01090"
      choose "Captain America" (isAbilityOfCard captain)
      payWith [strength]
      pass
      shields <- filter ((== "53034") . toCardCode) <$> handOf p
      length shields `shouldBe` 1
      map pcOwner shields `shouldBe` [Just p]
      getSetAsidePlayerCards >>= (`shouldSatisfy` all ((/= "53034") . toCardCode))
      villainDamageNow `shouldReturn` 0

  it "Captain America returns his Shield from play and deals four damage" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      captain <- putIntoPlay p "53023"
      _ <- putIntoPlay p "53034"
      strength <- putInHand p "01090"
      choose "Captain America" (isAbilityOfCard captain)
      payWith [strength]
      chooseVillain
      pass
      map toCardCode <$> handOf p >>= (`shouldSatisfy` elem "53034")
      upgradesWith "53034" >>= (`shouldSatisfy` null)
      villainDamageNow `shouldReturn` 4

  it "the Captain America shield grants defense and retaliate to its controller" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      _ <- putIntoPlay p "53023"
      _ <- putIntoPlay p "53034"
      modifiers <- getModifiers p
      modifiers `shouldSatisfy` elem (DefenseModifier 1)
      modifiers `shouldSatisfy` elem (KeywordModifier $ Retaliate 1)

  it "Strength in Diversity counts distinct friendly traits and resolves one point per trait" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      event <- putInHand p "53019"
      resources <- putInHand p "01088"
      playWith event [resources]
      chooseVillain
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  it "Winter Soldier ally removes threat after defeating a minion" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      bucky <- putIntoPlay p "53035"
      mid <- addMinion p Cards.fw53032
      placeMainThreat 3
      choose "Winter Soldier attacks" (abilityOn (allyIdOf bucky) 301)
      choose "minion" (isTargetChoice mid)
      pass
      -- Three-hit-point Serpent Soldier survives the first attack.
      mainSchemeThreatNow `shouldReturn` 3
      resolve [AllyMessage (allyIdOf bucky) ReadiedAlly]
      choose "Winter Soldier attacks again" (abilityOn (allyIdOf bucky) 301)
      choose "minion" (isTargetChoice mid)
      choose "remove threat response" (abilityOn (allyIdOf bucky) 1)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 1

  it "Resource Reserve tucks a resource, never an ordinary event" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      reserve <- putIntoPlay p "53021"
      energy <- putInHand p "01088"
      _ <- putInHand p "53003"
      choose "Resource Reserve" (isAbilityOfCard reserve)
      choose "tuck resource" (isTargetChoice energy)
      pass
      map toCardCode <$> handOf p >>= (`shouldSatisfy` notElem "01088")
      supports <- supportsWith "53021"
      map (map toCardCode . supportHeldCards) supports `shouldBe` [["01088"]]

  it "Resource Reserve uses a tucked Power of Leadership's two resources for a Leadership ally" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      reserve <- putIntoPlay p "53021"
      power <- putInHand p "01072"
      choose "tuck resource" (abilityOn (supportOf reserve) 1)
      choose "Power of Leadership" (isTargetChoice power)
      pass
      aero <- putInHand p "53015"
      other <- putInHand p "01086"
      length <$> getAvailableResourcesFor p (Just aero) `shouldReturn` 3
      choose "Aero is affordable using the stored conditional resources" (isPlay aero)
      choose "spend Resource Reserve" (abilityOn (supportOf reserve) 2)
      declareWildAs Energy
      declareWildAs Energy
      payWith [other]
      pass
      selectAny (AllyWithId $ allyIdOf aero) `shouldReturn` True
      held <- supportsWith "53021"
      map supportHeldCards held `shouldBe` [[]]
      map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "01072")

  it "Resource Reserve triggers a stored Avian Anatomy's after-spend return exactly once" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      reserve <- putIntoPlay p "53021"
      avian <- putInHand p "42008"
      choose "tuck Avian Anatomy" (abilityOn (supportOf reserve) 1)
      choose "stored resource" (isTargetChoice avian)
      pass
      event <- putInHand p "53003"
      other <- putInHand p "01086"
      choose "play Aerial event" (isPlay event)
      choose "spend the stored resource" (abilityOn (supportOf reserve) 2)
      declareWildAs Energy
      payWith [other]
      choose "Avian Anatomy response" (isLabel "$label.avianAnatomy")
      offers (isLabel "$label.avianAnatomy") `shouldReturn` False
      chooseVillain
      choose "leave encounter top" (isLabel "$label.doNotDiscard")
      pass
      handOf p >>= (`shouldSatisfy` elem event)
      discarded <- discardOf p
      length (filter ((== pcCardId avian) . pcCardId) discarded) `shouldBe` 1

  it "another player spends Resource Reserve's conditional resources while its card returns to its owner" do
    tg <- testGame ["falcon", "winter-soldier"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      asFalcon p
      reserve <- putIntoPlay p "53021"
      power <- putInHand p "01072"
      choose "tuck Power of Leadership" (abilityOn (supportOf reserve) 1)
      choose "stored resource" (isTargetChoice power)
      pass
      choose "end Falcon's turn" isEndTurn
      changeForm q
      pass
      clearHand q
      aero <- putInHand q "53015"
      other <- putInHand q "01086"
      choose "play Aero using another player's Reserve" (isPlay aero)
      choose "spend shared Resource Reserve" (abilityOn (supportOf reserve) 2)
      declareWildAs Energy
      declareWildAs Energy
      payWith [other]
      pass
      selectAny (AllyWithId (allyIdOf aero) <> AllyControlledBy (IdentityWithId q)) `shouldReturn` True
      ownerDiscard <- discardOf p
      length (filter ((== pcCardId power) . pcCardId) ownerDiscard) `shouldBe` 1
      discardOf q >>= (`shouldSatisfy` all ((/= pcCardId power) . pcCardId))

  it "Spectrum retains one card spent to pay for her and gains its printed bonus" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      spectrum <- putInHand p "53018"
      energy <- putInHand p "01088"
      others <- traverse (putInHand p) ["01089", "01086"]
      playWith spectrum (energy : others)
      choose "Spectrum" (abilityOn (allyIdOf spectrum) 1)
      choose "tuck Energy" (isTargetChoice energy)
      pass
      getModifiers (allyIdOf spectrum) >>= (`shouldSatisfy` elem (HitPointModifier 2))
      tucked <- allyHeldCards <$> allyIn "53018"
      map toCardCode tucked `shouldBe` ["01088"]

  it "Misty Knight has the Hero for Hire trait" do
    tg <- testGame ["falcon"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "53036"
      selectAny (AllyWithId (allyIdOf ally) <> AllyWithTrait T.HeroForHire) `shouldReturn` True

  it "Ops Room starts with three alert counters" do
    tg <- testGame ["falcon"]
    inGame tg do
      room <- putIntoPlay (player1 tg) "53037"
      getCountersOn (SupportRef $ SupportId $ unCardId $ pcCardId room) `shouldReturn` 3

  it "Hugin and Munin put a searched minion into play and ready one character per icon" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      ally <- putIntoPlay p "01083"
      resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf ally) ExhaustedAlly]
      cards <- stackEncounterDeck [Minions.shocker]
      birds <- putInHand p "53017"
      energy <- putInHand p "01088"
      playWith birds [energy]
      choose "Hugin and Munin" (abilityOn (allyIdOf birds) 1)
      choose "search minion" (isTargetChoice $ testHead cards)
      choose "ready Falcon" (isTargetChoice p)
      choose "ready ally" (isTargetChoice $ allyIdOf ally)
      pass
      isExhaustedNow p `shouldReturn` False
      allyExhausted <$> allyIn "01083" `shouldReturn` False
      selectAny (MinionIs Minions.shocker) `shouldReturn` True

  it "Wingman exhausts its host to prevent another Aerial ally's consequential damage" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      bird <- putIntoPlay p "53002"
      aero <- putIntoPlay p "53015"
      wingman <- putInHand p "53024"
      playOn wingman (allyIdOf bird) []
      pass
      choose "Aero attacks" (abilityOn (allyIdOf aero) 301)
      chooseVillain
      choose "Wingman" (abilityOn (upgradeOf wingman) 1)
      choose "exhaust Wingman host" (isTargetChoice $ allyIdOf bird)
      pass
      allyDamage <$> allyIn "53015" `shouldReturn` 0
      allyExhausted <$> allyIn "53002" `shouldReturn` True

  it "Ops Room prevents one defending damage and removes one threat" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      asFalcon p
      room <- putIntoPlay p "53037"
      mid <- addMinion p Minions.hydraSoldier
      placeMainThreat 3
      runNow [MinionMessage mid $ MinionAttacksWithChange p 1 []]
      choose "defend" isDefend
      choose "Ops Room" (abilityOn (supportOf room) 1)
      chooseMainScheme
      pass
      damageOf p `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` 2
      getCountersOn (supportOf room) `shouldReturn` 2
