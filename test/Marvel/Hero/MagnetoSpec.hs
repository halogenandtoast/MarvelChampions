module Marvel.Hero.MagnetoSpec (spec) where

import TestImport
import Marvel.Ability qualified as Ability
import Marvel.Message.Lifted qualified as L

asMagneto :: IdentityId -> GameAppT ()
asMagneto p = changeForm p >> pass >> clearHand p

isAbility :: IdentityId -> Natural -> Choice -> Bool
isAbility p n = \case
  UseAbility a -> abilitySource a == toSource p && abilityIndex a == n
  _ -> False

damagePlayer :: IdentityId -> Natural -> GameAppT ()
damagePlayer p n = L.runQueueT (L.capture $ L.damageIdentity p GameRef $ toDamage n FromAbility) >>= start

mandatory :: GameAppT ()
mandatory = do
  pass
  cs <- asked
  case cs >>= headMay . filter forced . snd of
    Just c -> choose "mandatory ability" (== c) >> mandatory
    Nothing -> pure ()
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse, Ability.WhenRevealed]
    _ -> False

spec :: Spec
spec = describe "Magneto" do
  it "Magnetic Pull discards through the first Magnetic card and adds it to hand" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asMagneto p
      _ <- stackDeck p ["01086", "49011", "49009"]
      choose "Magnetic Pull" (isAbility p 1)
      pass
      map toCardCode <$> handOf p `shouldReturn` ["49011"]
      map toCardCode <$> discardOf p `shouldReturn` ["01086"]
      offers (isAbility p 1) `shouldReturn` False

  it "Survivor shuffles precisely the top three discards when changing to Erik" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asMagneto p
      cards <- traverse (putInDiscard p) ["49002", "49003", "49004", "49005"]
      runNow [IdentityMessage p $ ChangedToForm B]
      choose "Survivor" (isAbility p 1)
      pass
      left <- discardOf p
      length left `shouldBe` 1
      take 1 cards `shouldBe` left

  it "Asteroid M shuffles the topmost Magnetic card and heals one damage" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asteroid <- putIntoPlay p "49002"
      old <- putInDiscard p "49009"
      top <- putInDiscard p "49011"
      _ <- putInDiscard p "01086"
      setDamage p 3
      choose "Asteroid M" (isAbilityOfCard asteroid)
      pass
      damageOf p `shouldReturn` 2
      discarded <- discardOf p
      old `elem` discarded `shouldBe` True
      top `elem` discarded `shouldBe` False

  it "Magneto's Helmet grants steady and generates a resource only for Magnetic cards" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asMagneto p
      helmet <- putIntoPlay p "49003"
      resolve [IdentityMessage p IdentityStunned]
      _ <- stackDeck p ["49011"]
      offers (isAbility p 1) `shouldReturn` True
      card <- putInHand p "49008"
      payment <- putInHand p "01086"
      placeMainThreat 4
      choose "Electromagnetic Blast" (isPlay card)
      choose "Helmet resource" (isAbilityOfCard helmet)
      payWith [payment]
      chooseMainScheme
      pass
      upgradeIsExhausted (upgradeIdOf helmet) `shouldReturn` True

  it "Magneto's Armor grants each matching stat once from discarded printed icons" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asMagneto p
      armor <- putIntoPlay p "49004"
      _ <- stackDeck p ["01089", "01090", "49011"]
      choose "Magnetic Pull" (isAbility p 1)
      choose "Armor" (isAbilityOfCard armor)
      pass
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "Magneto's Cape grants aerial and exhausts to ready after Magnetic Pull" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asMagneto p
      cape <- putIntoPlay p "49005"
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- stackDeck p ["49011"]
      choose "Magnetic Pull" (isAbility p 1)
      choose "Cape" (isAbilityOfCard cape)
      pass
      exhaustedNow p `shouldReturn` False
      upgradeIsExhausted (upgradeIdOf cape) `shouldReturn` True

  it "Magnetic Bubble absorbs a whole damage instance and breaks at six damage" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asMagneto p
      bubble <- putIntoPlay p "49006"
      damagePlayer p 4
      mandatory
      damageOf p `shouldReturn` 0
      upgradeUses . testHead <$> upgradesWith "49006" `shouldReturn` 4
      damagePlayer p 5
      mandatory
      damageOf p `shouldReturn` 0
      upgradesWith "49006" `shouldReturn` []
      (bubble `elem`) <$> discardOf p `shouldReturn` True
