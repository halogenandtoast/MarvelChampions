module Marvel.HealingSpec (spec) where

import TestImport
import Marvel.Minion.Cards qualified as Minions
import Marvel.Effect.Types qualified as Effect
import Marvel.Effect.Lasting (Lasting (..))
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Modifier (Modifier (CannotBeHealedByPlayerCards, RecoveryPenalty))

healingAbility :: PlayerCard -> Choice -> Bool
healingAbility card choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

blockHealing :: IdentityId -> GameAppT ()
blockHealing p = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect.Effect (Lasting $ Effect.EffectAttrs
    eid "engine-lasting" GameRef (IdentityEntity $ IdentityWithId p) [CannotBeHealedByPlayerCards] Nothing)

spec :: Spec
spec = describe "Healing eligibility" do
  for_ [("Queen Ramonda", "51008"), ("Aunt May", "01006"), ("Crew Quarters", "20029"), ("Med Team", "01080"), ("Mantis", "19002")] \(name, code) -> do
    it (name <> " is unavailable without damage to heal") do
      tg <- testGame ["black-panther-shuri"]
      inGame tg do
        let p = player1 tg
        clearHand p
        card <- putIntoPlay p code
        offers (healingAbility card) `shouldReturn` False

    it (name <> " is unavailable when its only damaged target cannot be healed") do
      tg <- testGame ["black-panther-shuri"]
      inGame tg do
        let p = player1 tg
        clearHand p
        card <- putIntoPlay p code
        setDamage p 3
        blockHealing p
        offers (healingAbility card) `shouldReturn` False

    it (name <> " remains available with damage it can heal") do
      tg <- testGame ["black-panther-shuri"]
      inGame tg do
        let p = player1 tg
        clearHand p
        card <- putIntoPlay p code
        setDamage p 3
        offers (healingAbility card) `shouldReturn` True

  it "Queen Ramonda cannot offer a recovery of zero" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      ramonda <- putIntoPlay p "51008"
      setDamage p 3
      eid <- getRandom
      withGame_ $ entitiesL . effectsL . at eid ?~ Effect.Effect (Lasting $ Effect.EffectAttrs
        eid "engine-lasting" GameRef (IdentityEntity $ IdentityWithId p) [RecoveryPenalty 100] Nothing)
      offers (isAbilityOfCard ramonda) `shouldReturn` False

  for_ [("First Aid", "01086"), ("Second Wind", "06033"), ("Recuperation", "15031")] \(name, code) -> do
    it (name <> " cannot be played when nobody needs healing") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        clearHand p
        card <- putInHand p code
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay card) `shouldReturn` False

    it (name <> " cannot be played when the only damaged character cannot heal") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        clearHand p
        card <- putInHand p code
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        setDamage p 3
        blockHealing p
        offers (isPlay card) `shouldReturn` False

  it "Project Rebirth keeps its draw option but hides blocked healing" do
    tg <- testGame ["venom"]
    inGame tg do
      let p = player1 tg
      clearHand p
      rebirth <- putIntoPlay p "20007"
      setDamage p 3
      blockHealing p
      choose "Project Rebirth" (isAbilityOfCard rebirth)
      offers (isLabel $ countLabel "healYourIdentity" 3) `shouldReturn` False
      offers (isLabel $ countLabel "drawCards" 1) `shouldReturn` True

  it "Wong can discard an invocation without offering healing at full health" do
    tg <- testGame ["doctor-strange"]
    inGame tg do
      let p = player1 tg
      clearHand p
      wong <- putIntoPlay p "09002"
      choose "Wong" (isAbilityOfCard wong)
      offers (isLabel $ countLabel "healYourIdentity" 1) `shouldReturn` False
      offers (isLabel "$label.discardTopInvocation") `shouldReturn` True

  it "The Iliad keeps its attack option without offering an empty heal" do
    tg <- testGame ["maria-hill"]
    inGame tg do
      let p = player1 tg
      clearHand p
      iliad <- putIntoPlay p "50009"
      choose "The Iliad" (isAbilityOfCard iliad)
      offers (isLabel "$label.heal") `shouldReturn` False
      offers (isLabel "$label.dealDamage") `shouldReturn` True

  it "The Night Nurse can remove a status even when healing is blocked" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      nurse <- putIntoPlay p "09019"
      blockHealing p
      resolve [GiveStatus (IdentityRef p) StunnedStatus]
      choose "The Night Nurse" (isAbilityOfCard nurse)
      choose "hero" (isTargetChoice p)
      choose "remove stun" (isLabel $ statusLabel StunnedStatus)
      pass
      identityMatches StunnedIdentity p `shouldReturn` False
      damageOf p `shouldReturn` 0

  it "The Night Nurse is unavailable without healing or a status to remove" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      nurse <- putIntoPlay p "09019"
      offers (isAbilityOfCard nurse) `shouldReturn` False

  it "Queen Ramonda offers another damaged Wakandan without offering her undamaged controller" do
    tg <- testGame ["black-panther-shuri", "black-panther"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      clearHand p
      ramonda <- putIntoPlay p "51008"
      setDamage other 4
      choose "Queen Ramonda" (isAbilityOfCard ramonda)
      offers (isTargetChoice p) `shouldReturn` False
      choose "T'Challa" (isTargetChoice other)
      pass
      damageOf other `shouldReturn` 0

  it "Rechannel cannot pay its healing cost through healing prevention" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      setDamage p 3
      _ <- putInHand p "01088"
      blockHealing p
      offers (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 1; _ -> False) `shouldReturn` False

  it "Preservation does not interrupt a card payment with blocked healing" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      setDamage p 3
      blockHealing p
      upgrade <- putInHand p "01081"
      preservation <- putInHand p "21064"
      playWith upgrade [preservation]
      offers (isLabel $ countLabel "healYourIdentity" 1) `shouldReturn` False
      pass
      damageOf p `shouldReturn` 3

  it "Steve's Apartment draws a card while respecting healing prevention" do
    tg <- testGame ["captain-america"]
    inGame tg do
      let p = player1 tg
      clearHand p
      apartment <- putIntoPlay p "03007"
      setDamage p 3
      blockHealing p
      before <- length <$> handOf p
      choose "Steve's Apartment" (isAbilityOfCard apartment)
      pass
      length <$> handOf p >>= (`shouldBe` (before + 1))
      damageOf p `shouldReturn` 3

  it "Reboot can still ready an undamaged Android when healing is blocked" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearHand p
      android <- putIntoPlay p "01068"
      resolve [AllyMessage (allyIdOf android) ExhaustedAlly]
      eid <- getRandom
      withGame_ $ entitiesL . effectsL . at eid ?~ Effect.Effect (Lasting $ Effect.EffectAttrs
        eid "engine-lasting" GameRef (AllyEntity $ AllyWithId $ allyIdOf android) [CannotBeHealedByPlayerCards] Nothing)
      reboot <- putInHand p "26024"
      resource <- putInHand p "01093"
      playOn reboot (AllyCharacter $ allyIdOf android) [resource]
      pass
      a <- allyIn "01068"
      allyExhausted a `shouldBe` False
      allyDamage a `shouldBe` 0

  it "Moment of Triumph is unavailable after defeating an enemy without excess damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      setDamage p 3
      mid <- addMinion p Minions.hydraBomber
      triumph <- putInHand p "12030"
      choose "basic attack" (isHeroAttack p)
      choose "Hydra Bomber" (isTargetChoice mid)
      offers (isPlay triumph) `shouldReturn` False
