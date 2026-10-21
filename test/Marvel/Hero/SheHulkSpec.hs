module Marvel.Hero.SheHulkSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Damage
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types (getMinionDamage)
import Marvel.Minion.Types qualified as M
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.SideScheme.Types (isCrisis)
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport hiding (minionDamageOf)

sheHulk :: IO TestGame
sheHulk = testGame ["she-hulk"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | A card put into play from hand, paid for with new resource cards.
inPlay :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
inPlay p code codes = do
  card <- putInHand p code
  playPaying p card codes
  pure card

isBasicAttack :: Choice -> Bool
isBasicAttack = \case
  UseAbility ab -> Attack `elem` abilityChoices ab
  _ -> False

isAnyPay :: Choice -> Bool
isAnyPay = \case
  PayWithCard _ -> True
  _ -> False

minionDamageOf :: EncounterCard -> GameAppT (Maybe Natural)
minionDamageOf c = getsGame (fmap getMinionDamage . HashMap.lookup (minionOf c) . gameMinions)

spec :: Spec
spec = describe "She-Hulk" do
  it "Do You Even Lift?: after changing to She-Hulk, deal 2 damage to an enemy" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      villainDamageNow `shouldReturn` 2

  it "Do You Even Lift? does not answer changing to Jennifer Walters" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      runNow [IdentityMessage p (ChangedToForm B)]
      offers (isNamedAbility "Do You Even Lift?") `shouldReturn` False

  it "I Object!: prevent 1 of the threat placed on a scheme, once a round" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      _ <- stackEncounterDeck [Minions.hydraMercenary]
      before <- mainSchemeThreat
      -- Rhino schemes for 1, and 1 boost
      _ <- revealNow p Treacheries.advance
      choose "I object" (isNamedAbility "\"I Object!\"")
      pass
      mainSchemeThreat `shouldReturn` (before + 1)
      _ <- stackEncounterDeck [Minions.hydraMercenary]
      _ <- revealNow p Treacheries.advance
      offers (isNamedAbility "\"I Object!\"") `shouldReturn` False
      pass
      mainSchemeThreat `shouldReturn` (before + 3)

  it "I Object! answers threat placed by an encounter card's ability" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putInHand p "01088"
      before <- mainSchemeThreat
      _ <- revealNow p Treacheries.theVulturesPlans
      choose "I object" (isNamedAbility "\"I Object!\"")
      pass
      mainSchemeThreat `shouldReturn` before

  it "Hellcat returns to her owner's hand" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      clearHand p
      hellcat <- inPlay p "01020" ["01088", "01089"]
      getsGame (HashMap.size . gameAllies) `shouldReturn` 1
      choose "return Hellcat" (isAbilityOfCard hellcat)
      getsGame (HashMap.size . gameAllies) `shouldReturn` 0
      map pcCardId <$> handOf p `shouldReturn` [pcCardId hellcat]

  it "Gamma Slam deals as much damage as she has sustained" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      clearHand p
      setDamage p 7
      slam <- putInHand p "01021"
      playPaying p slam ["01088", "01089"]
      vid <- villainIdNow
      choose "slam Rhino" (isTargetChoice vid)
      villainDamageNow `shouldReturn` 9

  it "Ground Stomp deals 1 damage to each enemy, and is not an attack" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      clearHand p
      strength <- inPlay p "01028" ["01089"]
      mercenary <- revealNow p Minions.hydraMercenary
      pass
      stomp <- putInHand p "01022"
      playPaying p stomp ["01088"]
      pass
      villainDamageNow `shouldReturn` 3
      minionDamageOf mercenary `shouldReturn` Just 1
      -- Superhuman Strength answers attacks only
      getsGame (HashMap.member (upgradeOf strength) . gameUpgrades) `shouldReturn` True

  it "Legal Practice: discard up to 5 cards to remove that much threat" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      mainScheme <- mainSchemeIdNow
      runNow [MainSchemeMessage mainScheme (MainSchemePlaceThreat 5)]
      before <- mainSchemeThreat
      clearHand p
      practice <- putInHand p "01023"
      junk <- traverse (putInHand p) ["01088", "01088", "01088"]
      choose "play Legal Practice" (isPlay practice)
      for_ junk \c -> choose "discard" (isTargetChoice c)
      choose "done discarding" (isLabel "Done")
      pass
      mainSchemeThreat `shouldReturn` (before - 3)
      length <$> discardOf p `shouldReturn` 4

  it "Legal Practice is an alter-ego action" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      practice <- putInHand p "01023"
      _ <- putInHand p "01088"
      offers (isPlay practice) `shouldReturn` False

  it "One-Two Punch readies She-Hulk after her basic attack" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      clearHand p
      punch <- putInHand p "01024"
      _ <- putInHand p "01088"
      choose "attack" isBasicAttack
      vid <- villainIdNow
      choose "attack Rhino" (isTargetChoice vid)
      isExhaustedNow p `shouldReturn` True
      choose "play One-Two Punch" (isPlay punch)
      whenM (offers isAnyPay) $ choose "pay" isAnyPay
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      pass
      isExhaustedNow p `shouldReturn` False
      villainDamageNow `shouldReturn` 5

  it "Split Personality changes her form and draws up to the printed hand size" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      clearHand p
      split <- putInHand p "01025"
      playPaying p split ["01088", "01089"]
      pass
      a <- playerAttrs p
      a.playerIdentitySide `shouldBe` A
      length <$> handOf p `shouldReturn` 4

  it "Superhuman Law Division: exhaust and spend a mental resource to remove 2 threat" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      mainScheme <- mainSchemeIdNow
      runNow [MainSchemeMessage mainScheme (MainSchemePlaceThreat 5)]
      before <- mainSchemeThreat
      clearHand p
      division <- inPlay p "01026" ["01090"]
      genius <- putInHand p "01089"
      choose "use the division" (isAbilityOfCard division)
      choose "pay with genius" (isPayWith genius)
      whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
      mainSchemeThreat `shouldReturn` (before - 2)
      _ <- putInHand p "01089"
      offers (isAbilityOfCard division) `shouldReturn` False

  it "Superhuman Law Division needs a mental resource and alter-ego form" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      mainScheme <- mainSchemeIdNow
      runNow [MainSchemeMessage mainScheme (MainSchemePlaceThreat 5)]
      clearHand p
      division <- inPlay p "01026" ["01090"]
      _ <- putInHand p "01090"
      offers (isAbilityOfCard division) `shouldReturn` False
      _ <- putInHand p "01089"
      changeForm p
      offers (isAbilityOfCard division) `shouldReturn` False

  it "Focused Rage: exhaust it and take 1 damage to draw a card" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      clearHand p
      rage <- inPlay p "01027" ["01088", "01089"]
      choose "use Focused Rage" (isAbilityOfCard rage)
      damageOf p `shouldReturn` 1
      length <$> handOf p `shouldReturn` 1
      offers (isAbilityOfCard rage) `shouldReturn` False

  it "Superhuman Strength gives +2 ATK, and is discarded to stun the enemy she attacks" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      clearHand p
      strength <- inPlay p "01028" ["01089"]
      choose "attack" isBasicAttack
      vid <- villainIdNow
      choose "attack Rhino" (isTargetChoice vid)
      choose "Superhuman Strength" (isAbilityOfCard strength)
      pass
      villainDamageNow `shouldReturn` 7
      (V.villainStunned . toAttrs <$> getVillain) `shouldReturn` True
      getsGame (HashMap.member (upgradeOf strength) . gameUpgrades) `shouldReturn` False

  it "Legal Work: flipping to Jennifer Walters and exhausting her removes it from the game" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      _ <- revealNow p Obligations.legalWork
      choose "flip to alter-ego" (isLabel "$label.changeToB")
      choose "exhaust Jennifer Walters" (isLabelStarting "$label.exhaust")
      isExhaustedNow p `shouldReturn` True
      getsGame (HashMap.null . gameObligations) `shouldReturn` True
      getsGame (scenarioAccelerationTokens . gameScenario) `shouldReturn` 0

  it "Legal Work: otherwise the main scheme gets an acceleration token" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      work <- revealNow p Obligations.legalWork
      choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
      pass
      getsGame (scenarioAccelerationTokens . gameScenario) `shouldReturn` 1
      (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` elem (ecCardId work))

  it "Personal Challenge is a crisis with 3 threat and 1 more per hero" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      challenge <- revealNow p SideSchemes.personalChallenge
      pass
      sideSchemeThreat (sideSchemeOf challenge) `shouldReturn` 4
      crisis <- getsGame (fmap isCrisis . HashMap.lookup (sideSchemeOf challenge) . gameSideSchemes)
      crisis `shouldBe` Just True

  it "Titania does not attack as she engages, and attacks for her remaining hit points" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      titania <- revealNow p Minions.titania
      pass
      damageOf p `shouldReturn` 0
      runNow [MinionMessage (minionOf titania) (MinionDamaged GameRef (toDamage 2 FromAbility))]
      runNow [MinionMessage (minionOf titania) (MinionAttacks p)]
      choose "no defender" isNoDefender
      pass
      damageOf p `shouldReturn` 4

  it "Genetically Enhanced gives the minion with the highest printed hit points +3, Titania attacks harder" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      _ <- revealNow p Minions.hydraMercenary
      pass
      titania <- revealNow p Minions.titania
      pass
      _ <- revealNow p Attachments.geneticallyEnhanced
      pass
      attachments <- getsGame (fmap (length . M.minionAttachments . toAttrs) . HashMap.lookup (minionOf titania) . gameMinions)
      attachments `shouldBe` Just 1
      runNow [MinionMessage (minionOf titania) (MinionDamaged GameRef (toDamage 7 FromAbility))]
      minionDamageOf titania `shouldReturn` Just 7
      runNow [MinionMessage (minionOf titania) (MinionAttacks p)]
      choose "no defender" isNoDefender
      pass
      damageOf p `shouldReturn` 2

  it "Genetically Enhanced surges when there is no minion" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      [next] <- stackEncounterDeck [Treacheries.advance]
      enhanced <- revealNow p Attachments.geneticallyEnhanced
      pass
      getsGame (HashMap.null . gameAttachments) `shouldReturn` True
      discard <- map ecCardId <$> encounterDiscard
      discard `shouldSatisfy` elem (ecCardId enhanced)
      discard `shouldSatisfy` elem (ecCardId next)

  it "Titania's Fury: Titania attacks the hero" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      _ <- revealNow p Minions.titania
      pass
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      _ <- revealNow p Treacheries.titaniasFury
      choose "no defender" isNoDefender
      pass
      damageOf p `shouldReturn` 6
      getsGame (HashMap.member (minionOf next) . gameMinions) `shouldReturn` False

  it "Titania's Fury: if she did not attack, she heals and it surges" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      titania <- revealNow p Minions.titania
      pass
      runNow [MinionMessage (minionOf titania) (MinionDamaged GameRef (toDamage 2 FromAbility))]
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      _ <- revealNow p Treacheries.titaniasFury
      pass
      damageOf p `shouldReturn` 0
      minionDamageOf titania `shouldReturn` Just 0
      getsGame (HashMap.member (minionOf next) . gameMinions) `shouldReturn` True

  it "Titania's Fury as a boost gives the villain another boost card" do
    tg <- sheHulk
    let p = player1 tg
    inGame tg do
      changeForm p
      choose "do you even lift" (isNamedAbility "Do You Even Lift?")
      _ <- stackEncounterDeck [Treacheries.titaniasFury, Treacheries.theVulturesPlans]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "no defender" isNoDefender
      pass
      -- 2 ATK, 1 boost on Titania's Fury and 2 on the extra card
      damageOf p `shouldReturn` 5
