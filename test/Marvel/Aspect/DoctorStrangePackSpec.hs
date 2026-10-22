module Marvel.Aspect.DoctorStrangePackSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Identity (isPlayable)
import Marvel.Villain.Types qualified as V
import TestImport

doctorStrange :: IO TestGame
doctorStrange = testGame ["doctor-strange"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | The villain attacks the player, its boost card a Vulture (2 boost icons).
villainAttacks :: IdentityId -> GameAppT ()
villainAttacks p = do
  _ <- stackEncounterDeck [Minions.vulture]
  vid <- villainIdNow
  start [VillainMessage vid (VillainAttacks p)]

isVillainChoice :: Choice -> Bool
isVillainChoice = \case
  TargetLabel (VillainRef _) _ -> True
  TargetLabel (EnemyRef (EnemyVillainId _)) _ -> True
  _ -> False

spec :: Spec
spec = describe "Doctor Strange pack" do
  it "Brother Voodoo searches the top 5 cards of the deck for an event as he enters play" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      [_, ward] <- stackDeck p ["09024", "09007"]
      voodoo <- putInHand p "09012"
      playPaying p voodoo ["01088", "01089"]
      choose "use Brother Voodoo" (isAbilityOfCard voodoo)
      choose "Protective Ward" (isTargetChoice ward)
      map toCardCode <$> handOf p `shouldReturn` ["09007"]

  it "Clea is shuffled into her owner's deck when defeated, not discarded" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      clea <- putIntoPlay p "09013"
      deckBefore <- length . unDeck . (.playerIdentityDeck) <$> playerAttrs p
      start [AllyMessage (allyIdOf clea) (AllyDamaged (toSource p) (toDamage 2 FromAbility))]
      choose "Clea" (isAbilityOfCard clea)
      resolve []
      alliesWith "09013" `shouldReturn` []
      map toCardCode <$> discardOf p >>= (`shouldSatisfy` notElem "09013")
      length . unDeck . (.playerIdentityDeck) <$> playerAttrs p `shouldReturn` deckBefore + 1

  it "a defeated ally is discarded once" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      clea <- putIntoPlay p "09013"
      start [AllyMessage (allyIdOf clea) (AllyDamaged (toSource p) (toDamage 2 FromAbility))]
      choose "decline" isNoResponse
      resolve []
      filter (== "09013") . map toCardCode <$> discardOf p `shouldReturn` ["09013"]

  it "Iron Fist removes a mystic counter as he attacks to stun the enemy and deal it 1 damage" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putIntoPlay p "09014"
      fist <- allyIn "09014"
      fist.allyCounters `shouldBe` 2
      choose "attack" (isAllyAttack fist)
      choose "Rhino" isVillainChoice
      choose "Iron Fist" (isAbilityOf (allyId fist))
      resolve []
      villainDamageNow `shouldReturn` 3
      (.allyCounters) <$> allyIn "09014" `shouldReturn` 1
      V.villainStunned . toAttrs <$> getVillain `shouldReturn` True

  it "Iron Fist takes no consequential damage when his interrupt defeats the enemy he attacks" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putIntoPlay p "09014"
      mercenary <- addMinion p Minions.hydraMercenary
      runNow [MinionMessage mercenary (MinionDamaged (toSource p) (toDamage 2 FromAbility))]
      fist <- allyIn "09014"
      choose "attack" (isAllyAttack fist)
      choose "Hydra Mercenary" (isTargetChoice mercenary)
      choose "Iron Fist" (isAbilityOf (allyId fist))
      resolve []
      minionDamageOf mercenary `shouldReturn` Nothing
      fist' <- allyIn "09014"
      fist'.allyDamage `shouldBe` 0
      fist'.allyExhausted `shouldBe` True

  it "Iron Fist takes consequential damage when the enemy survives his interrupt" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putIntoPlay p "09014"
      fist <- allyIn "09014"
      choose "attack" (isAllyAttack fist)
      choose "Rhino" isVillainChoice
      choose "Iron Fist" (isAbilityOf (allyId fist))
      resolve []
      (.allyDamage) <$> allyIn "09014" `shouldReturn` 1

  it "Desperate Defense gives +2 DEF for the attack, and readies the hero who takes no damage" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      defense <- putInHand p "09015"
      _ <- putInHand p "01088"
      -- Rhino's 2 ATK and 2 boost against Doctor Strange's 2 DEF and 2 more
      villainAttacks p
      choose "defend" isDefend
      choose "Desperate Defense" (isPlay defense)
      chooseIf \case PayWithCard _ -> True; _ -> False
      chooseIf isFinishPayment
      resolve []
      damageOf p `shouldReturn` 0
      isExhaustedNow p `shouldReturn` False

  it "Momentum Shift heals 2 damage from your hero, which it needs, to deal 2 damage" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      shift <- putInHand p "09016"
      _ <- putInHand p "01088"
      setDamage p 1
      offers (isPlay shift) `shouldReturn` False
      setDamage p 3
      choose "play" (isPlay shift)
      chooseIf \case PayWithCard _ -> True; _ -> False
      chooseIf isFinishPayment
      chooseIf isVillainChoice
      damageOf p `shouldReturn` 1
      villainDamageNow `shouldReturn` 2

  it "The Night Nurse heals 1 damage from a hero and discards a status card from them" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      setDamage p 2
      resolve [IdentityMessage p IdentityStunned]
      nurse <- putIntoPlay p "09019"
      choose "The Night Nurse" (isAbilityOfCard nurse)
      chooseIf (isTargetChoice p)
      chooseIf (isLabel "$label.stunnedStatus")
      damageOf p `shouldReturn` 1
      isStunned p `shouldReturn` False

  it "Unflappable draws a card after you defend and take no damage" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      unflappable <- putInHand p "09020"
      playPaying p unflappable ["01089"]
      _ <- putInHand p "09015"
      _ <- putInHand p "01088"
      villainAttacks p
      choose "defend" isDefend
      choose "Desperate Defense" \case TargetLabel _ cs -> any (\case PlayCard c _ -> toCardCode c == "09015"; _ -> False) cs; _ -> False
      chooseIf \case PayWithCard _ -> True; _ -> False
      chooseIf isFinishPayment
      before <- length <$> handOf p
      chooseIf (isAbilityOfCard unflappable)
      length <$> handOf p `shouldReturn` before + 1

  it "Warning reduces the damage a hero would take by 1" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      warning <- putInHand p "09021"
      start (damageIdentity p (toSource p) (toDamage 3 FromAbility))
      choose "Warning" (isPlay warning)
      resolve []
      damageOf p `shouldReturn` 2

  it "The Sorcerer Supreme is played only with the Mystic trait, and gives +1 hand size in hero form" do
    tg <- testGame ["doctor-strange", "spider-man"]
    let (p, other) = (player1 tg, player2 tg)
    inGame tg do
      clearHand p
      size <- handSizeOf p
      supreme <- putInHand p "09026"
      playPaying p supreme ["01089"]
      handSizeOf p `shouldReturn` size
      changeForm p
      heroSize <- handSizeOf p
      -- Doctor Strange's printed hand size is 5
      heroSize `shouldBe` 6
      -- Spider-Man has no Mystic trait
      _ <- putInHand other "01089"
      peter <- putInHand other "09026"
      (getPlayer other >>= (`isPlayable` peter)) `shouldReturn` False

  it "Skilled Strike gives +2 ATK to a basic attack" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      strike <- putInHand p "09037"
      choose "attack" (isHeroAttack p)
      choose "Skilled Strike" (isPlay strike)
      choose "Rhino" isVillainChoice
      resolve []
      -- Doctor Strange's printed ATK is 1
      villainDamageNow `shouldReturn` 3

  it "Foiled! cancels the boost icons of a boost card turned up as the villain schemes" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      foiled <- putInHand p "09038"
      _ <- stackEncounterDeck [Minions.vulture]
      threat <- mainSchemeThreatNow
      vid <- villainIdNow
      start [VillainMessage vid VillainSchemes]
      choose "Foiled!" (isPlay foiled)
      resolve []
      -- Rhino's 1 scheme, without the 2 boost
      mainSchemeThreatNow `shouldReturn` (threat + 1)

  it "Foiled! is not played as the villain attacks" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      foiled <- putInHand p "09038"
      villainAttacks p
      choose "no defender" isNoDefender
      offers (isPlay foiled) `shouldReturn` False

  it "Iron Man (Tony Stark) is an ally" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      ironMan <- putInHand p "09039"
      playPaying p ironMan ["01088", "01089"]
      length <$> alliesWith "09039" `shouldReturn` 1
