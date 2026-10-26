module Marvel.Hero.ScarletWitchSpec (spec) where

import Marvel.Attack (attack)
import Marvel.Keyword (Keyword (Overkill))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types (Villain (..), attackingL, overVillainAttrs)
import TestImport

scarletWitch :: IO TestGame
scarletWitch = testGame ["scarlet-witch"]

-- | Scarlet Witch in hero form, her hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  pass
  clearHand p

isChaosControl :: Choice -> Bool
isChaosControl = isNamedAbility "Chaos Control"

-- | The ally's own action, not his basic powers.
isReadyQuicksilver :: PlayerCard -> Choice -> Bool
isReadyQuicksilver qs c = isAbilityOfCard qs c && case c of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

isSiblings :: Choice -> Bool
isSiblings = isNamedAbility "Superpowered Siblings"

isIncrease, isDecrease :: Choice -> Bool
isIncrease = isLabel "$label.increaseBoostIcons"
isDecrease = isLabel "$label.decreaseBoostIcons"

-- | Rhino attacks the player, who does not defend.
rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]
  choose "no defender" isNoDefender

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . (.playerIdentityDeck) <$> playerAttrs p

spec :: Spec
spec = describe "Scarlet Witch" do
  it "deals the pack's own deck of 40 cards" do
    sum (map snd (starter "scarlet-witch").cards) `shouldBe` (40 :: Int) :: IO ()

  describe "Chaos Control" do
    it "counts the boost icons of the next encounter card instead, discarding it" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        -- Sound Manipulation (2 boost icons) is the boost card, Stampede (1) is next
        _ <- stackEncounterDeck [Treacheries.soundManipulation, Treacheries.stampede]
        rhinoAttacks p
        choose "Chaos Control" isChaosControl
        pass
        -- Rhino's 2 ATK and Stampede's 1
        damageOf p `shouldReturn` 3
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01106")

    it "can raise the count" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- stackEncounterDeck [Treacheries.advance, Treacheries.soundManipulation]
        rhinoAttacks p
        choose "Chaos Control" isChaosControl
        pass
        damageOf p `shouldReturn` 4

    -- the encounter deck is never empty: emptied, it is made again from the discard pile
    it "discards from the deck made again when the boost card emptied it" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- setEncounterDeck [Treacheries.soundManipulation] [Treacheries.stampede]
        rhinoAttacks p
        choose "Chaos Control" isChaosControl
        pass
        damageOf p `shouldReturn` 3

    it "counts The Next Evolution's extra boost icon on the card it discards, and the Crest adds to that" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        crest <- putIntoPlay p "15009"
        _ <- revealNow p SideSchemes.theNextEvolution
        pass
        _ <- stackEncounterDeck [Treacheries.soundManipulation, Treacheries.stampede]
        rhinoAttacks p
        choose "Chaos Control" isChaosControl
        choose "Crest" (isAbilityOfCard crest)
        choose "increase" isIncrease
        pass
        -- Rhino's 2 ATK, Stampede's 1 and The Next Evolution's 1, and the Crest's 1
        damageOf p `shouldReturn` 5

    it "is limited to once per phase" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- stackEncounterDeck [Treacheries.soundManipulation, Treacheries.stampede]
        rhinoAttacks p
        choose "Chaos Control" isChaosControl
        pass
        _ <- stackEncounterDeck [Treacheries.soundManipulation, Treacheries.stampede]
        rhinoAttacks p
        offers isChaosControl `shouldReturn` False
        pass
        -- 3, then 4
        damageOf p `shouldReturn` 7

    it "is not offered in alter-ego form" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- stackEncounterDeck [Treacheries.soundManipulation, Treacheries.stampede]
        vid <- villainIdNow
        runNow [VillainMessage vid VillainSchemes]
        offers isChaosControl `shouldReturn` False

  describe "Wanda Maximoff: Superpowered Siblings" do
    it "discards 2 cards to draw 2, once a round" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        a <- putInHand p "01088"
        b <- putInHand p "01089"
        _ <- putInHand p "01090"
        choose "Superpowered Siblings" isSiblings
        choose "discard" (isTargetChoice a)
        choose "discard" (isTargetChoice b)
        pass
        length <$> handOf p `shouldReturn` 3
        discardOf p >>= (`shouldSatisfy` \d -> a `elem` d && b `elem` d)
        offers isSiblings `shouldReturn` False

    it "draws 3 instead with the Quicksilver ally in play" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "15002"
        a <- putInHand p "01088"
        b <- putInHand p "01089"
        _ <- putInHand p "01090"
        choose "Superpowered Siblings" isSiblings
        choose "discard" (isTargetChoice a)
        choose "discard" (isTargetChoice b)
        pass
        length <$> handOf p `shouldReturn` 4

    it "draws 3 instead with Pietro Maximoff as another player's identity" do
      tg <- testGame ["scarlet-witch", "quicksilver"]
      let p = player1 tg
      inGame tg do
        clearHand p
        a <- putInHand p "01088"
        b <- putInHand p "01089"
        _ <- putInHand p "01090"
        choose "Superpowered Siblings" isSiblings
        choose "discard" (isTargetChoice a)
        choose "discard" (isTargetChoice b)
        pass
        length <$> handOf p `shouldReturn` 4

    it "needs 2 cards to discard" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01088"
        offers isSiblings `shouldReturn` False

  describe "Quicksilver — Pietro Maximoff (15002)" do
    it "readies himself, once a phase, only while exhausted" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        qs <- putIntoPlay p "15002"
        offers (isReadyQuicksilver qs) `shouldReturn` False
        resolve [AllyMessage (allyIdOf qs) ExhaustedAlly]
        choose "ready Quicksilver" (isReadyQuicksilver qs)
        map (.allyExhausted) <$> alliesWith "15002" `shouldReturn` [False]
        resolve [AllyMessage (allyIdOf qs) ExhaustedAlly]
        offers (isReadyQuicksilver qs) `shouldReturn` False

  describe "Agatha Harkness (15007)" do
    it "adds 1 of the top 3 cards to hand and puts the rest on the bottom in the order chosen" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        agatha <- putIntoPlay p "15007"
        [a, b, c] <- stackDeck p ["01088", "01089", "01090"]
        choose "use Agatha Harkness" (isAbilityOfCard agatha)
        choose "take" (isTargetChoice b)
        choose "bottom first" (isTargetChoice c)
        pass
        handOf p `shouldReturn` [b]
        deck <- deckOf p
        liftIO $ drop (length deck - 2) deck `shouldBe` [c, a]
        map (.supportExhausted) <$> supportsWith "15007" `shouldReturn` [True]

    it "is an alter-ego action" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        agatha <- putIntoPlay p "15007"
        offers (isAbilityOfCard agatha) `shouldReturn` False

  describe "Magic Shield (15008)" do
    it "prevents 3 of the damage of an attack on the hero, and is discarded" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        shield <- putIntoPlay p "15008"
        _ <- stackEncounterDeck [Treacheries.soundManipulation]
        rhinoAttacks p
        -- Chaos Control is offered on the boost card first
        choose "decline Chaos Control" isNoResponse
        choose "Magic Shield" (isAbilityOfCard shield)
        pass
        damageOf p `shouldReturn` 1
        length <$> upgradesWith "15008" `shouldReturn` 0

    it "prevents 3 of the damage to an ally" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        shield <- putIntoPlay p "15008"
        qs <- putIntoPlay p "15002"
        runNow (damageAlly (allyIdOf qs) GameRef (toDamage 4 FromAbility))
        choose "Magic Shield" (isAbilityOfCard shield)
        pass
        map (.allyDamage) <$> alliesWith "15002" `shouldReturn` [1]

    it "prevents 3 of the damage to another player's hero" do
      tg <- testGame ["scarlet-witch", "spider-man"]
      let p1 = player1 tg
          p2 = player2 tg
      inGame tg do
        asHero p1
        shield <- putIntoPlay p1 "15008"
        runNow (damageIdentity p2 GameRef (toDamage 4 FromAbility))
        choose "Magic Shield" (isAbilityOfCard shield)
        pass
        damageOf p2 `shouldReturn` 1

    it "prevents 3 of the overkill damage an attack on an ally deals its controller" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        shield <- putIntoPlay p "15008"
        qs <- putIntoPlay p "15002"
        runNow [AllyMessage (allyIdOf qs) (AllyDamaged GameRef (toDamage 3 FromAbility))]
        _ <- stackEncounterDeck [Treacheries.advance]
        vid <- villainIdNow
        -- the attack declared, and overkill gained there (as the Rise of Red Skull spec)
        withGame_ $ entitiesL . villainsL . ix vid %~ \(Villain v) -> Villain $ overVillainAttrs (attackingL ?~ attack (EnemyVillainId vid) (IdentityCharacter p) 0) v
        start [VillainMessage vid (VillainAttackGains Overkill), VillainMessage vid (VillainBeginAttack p), VillainMessage vid VillainEndAttack]
        choose "Quicksilver defends" \case AllyDefend aid _ -> aid == allyIdOf qs; _ -> False
        choose "decline Chaos Control" isNoResponse
        choose "no Magic Shield for Quicksilver" isNoResponse
        -- Rhino's 2 ATK: 1 defeats Quicksilver, 1 is the overkill, which the shield answers
        choose "Magic Shield" (isAbilityOfCard shield)
        pass
        damageOf p `shouldReturn` 0
        length <$> upgradesWith "15008" `shouldReturn` 0

    it "is a hero interrupt" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        shield <- putIntoPlay p "15008"
        runNow (damageIdentity p GameRef (toDamage 4 FromAbility))
        offers (isAbilityOfCard shield) `shouldReturn` False

  describe "Scarlet Witch's Crest (15009)" do
    it "adds 1 boost icon to a villain's boost card in alter-ego form" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        clearHand p
        crest <- putIntoPlay p "15009"
        _ <- stackEncounterDeck [Treacheries.soundManipulation]
        threat <- mainSchemeThreatNow
        vid <- villainIdNow
        runNow [VillainMessage vid VillainSchemes]
        choose "Crest" (isAbilityOfCard crest)
        choose "increase" isIncrease
        pass
        -- Rhino's 1 SCH and 3 boost icons
        mainSchemeThreatNow `shouldReturn` (threat + 4)
        map (.upgradeExhausted) <$> upgradesWith "15009" `shouldReturn` [True]

    it "takes 1 boost icon away" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        crest <- putIntoPlay p "15009"
        _ <- stackEncounterDeck [Treacheries.soundManipulation]
        rhinoAttacks p
        choose "Crest" (isAbilityOfCard crest)
        choose "decrease" isDecrease
        pass
        damageOf p `shouldReturn` 3

    it "does not take away from no boost icons" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        crest <- putIntoPlay p "15009"
        _ <- stackEncounterDeck [Treacheries.advance]
        rhinoAttacks p
        choose "Crest" (isAbilityOfCard crest)
        offers isDecrease `shouldReturn` False
        choose "increase" isIncrease
        pass
        damageOf p `shouldReturn` 3

    it "used after Chaos Control, changes the new count" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        crest <- putIntoPlay p "15009"
        _ <- stackEncounterDeck [Treacheries.soundManipulation, Treacheries.stampede]
        rhinoAttacks p
        choose "Chaos Control" isChaosControl
        choose "Crest" (isAbilityOfCard crest)
        choose "increase" isIncrease
        pass
        -- Rhino's 2 ATK, Stampede's 1 and the Crest's 1
        damageOf p `shouldReturn` 4

    it "used before Chaos Control, is replaced by its count" do
      tg <- scarletWitch
      let p = player1 tg
      inGame tg do
        asHero p
        crest <- putIntoPlay p "15009"
        _ <- stackEncounterDeck [Treacheries.soundManipulation, Treacheries.stampede]
        rhinoAttacks p
        choose "Crest" (isAbilityOfCard crest)
        choose "increase" isIncrease
        choose "Chaos Control" isChaosControl
        pass
        -- Rhino's 2 ATK and Stampede's 1
        damageOf p `shouldReturn` 3
