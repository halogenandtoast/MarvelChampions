module Marvel.Rules.ChosenHandCardCostSpec (spec) where

import Marvel.Card.Side (Side (A))
import Marvel.ActiveCost.Types
import Marvel.Cost (canAffordAllianceCost)
import Marvel.Cost.Types
import Marvel.Matchers.Types hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Query
import Marvel.Payment.Types
import Marvel.Trait (Trait (Avenger, Guardian))
import TestImport

reservedAttack :: IdentityId -> Int -> GameAppT PlayerCard
reservedAttack p n = do
  original <- putInHand p "01005"
  let card = original {pcCardDef = (getCardDef original)
        {cdCost = Just n, cdAbilityCost = ChosenHandCardCost}}
  withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityHand = Hand (card : filter (/= original) (unHand a.playerIdentityHand))}
  pure card

spec :: Spec
spec = describe "chosen cards and choose-only character costs" do
  it "keeps the chosen hand card in hand and excludes it from resource payments" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      source <- reservedAttack p 1
      selected <- putInHand p "01068"
      energy <- putInHand p "01088"
      runNow [IdentityMessage p (PayForCard source Nothing (Just $ toRef selected))]
      handOf p >>= (`shouldSatisfy` elem selected)
      offers (isPayWith selected) `shouldReturn` False
      offers (isPayWith energy) `shouldReturn` True
      choose "pay from another card" (isPayWith energy)
      handOf p >>= (`shouldSatisfy` elem selected)
      chooseVillain
      pass
      discardOf p >>= (`shouldSatisfy` notElem selected)

  it "cannot finance an event with the card that its choose-card cost reserved" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      source <- reservedAttack p 1
      selected <- putInHand p "01068"
      runNow [IdentityMessage p (PayForCard source Nothing (Just $ toRef selected))]
      offers (isPayWith selected) `shouldReturn` False
      handOf p >>= (`shouldSatisfy` elem selected)
      villainDamageNow `shouldReturn` 0

  it "cancelling a partial payment restores spent cards without duplicating its hand target" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      source <- reservedAttack p 3
      selected <- putInHand p "01068"
      energy <- putInHand p "01088"
      _ <- putInHand p "01089"
      runNow [IdentityMessage p (PayForCard source Nothing (Just $ toRef selected))]
      choose "pay two" (isPayWith energy)
      Just acid <- getActiveCost
      runNow [AbortPayment acid]
      cards <- handOf p
      length (filter ((== pcCardId selected) . pcCardId) cards) `shouldBe` 1
      cards `shouldSatisfy` elem energy
      cards `shouldSatisfy` elem source

  it "choose-only costs can select another player's exhausted character without spending it" do
    tg <- testGame ["captain-marvel", "rocket-raccoon"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      changeForm p
      resolve [IdentityMessage other (ChangedToForm A)]
      resolve [IdentityMessage other ExhaustedIdentity]
      acid <- getRandom
      let cost = ChooseCharacterWithTraitCost Guardian AnyCharacter
      withGame_ $ activeCostL .~ [ActiveCost acid p ForTreachery cost NoPayment Nothing [] Nothing]
      runNow [PayCost acid cost]
      paymentChosenCharacters <$> getCurrentPayment `shouldReturn` [IdentityCharacter other]
      playerIdentityExhausted . toAttrs <$> getPlayer other `shouldReturn` True
      fmap fst <$> asked >>= (`shouldSatisfy` (/= Just other))

  it "requires distinct characters even when a single ally has both traits" do
    tg <- testGame ["star-lord"]
    let p = player1 tg
    inGame tg do
      changeForm p
      -- Star-Lord gives Vision Guardian as well as its printed Avenger trait.
      _ <- putIntoPlay p "01068"
      -- A pair exists because Star-Lord supplies a second Guardian.
      canAffordAllianceCost [p] GameRef
        (ChooseCharacterWithTraitCost Avenger AnyCharacter <> ChooseCharacterWithTraitCost Guardian AnyCharacter)
        `shouldReturn` True
      -- Without Vision there is no Avenger at all; neither exhaustion nor
      -- somebody's ability to contribute resources can invent the missing pair.
      [vision] <- alliesWith "01068"
      resolve [RemoveFromPlay $ toRef vision]
      canAffordAllianceCost [p] GameRef
        (ChooseCharacterWithTraitCost Avenger AnyCharacter <> ChooseCharacterWithTraitCost Guardian AnyCharacter)
        `shouldReturn` False

  it "uses printed ally cost and only offers exhausted characters that can actually ready" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01068"
      selectAny (AllyWithId (allyIdOf ally) <> AllyWithPrintedCostAtLeast 4) `shouldReturn` True
      selectAny (AllyWithId (allyIdOf ally) <> AllyWithPrintedCostAtLeast 5) `shouldReturn` False
      selectAny (AllyWithId (allyIdOf ally) <> AllyCanReady) `shouldReturn` True
      selectList CharacterCanBeReadied >>= (`shouldSatisfy` notElem (AllyCharacter $ allyIdOf ally))
      resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
      selectList CharacterCanBeReadied >>= (`shouldSatisfy` elem (AllyCharacter $ allyIdOf ally))

  it "constrains each chosen character and never reuses one character for two costs" do
    tg <- testGame ["captain-marvel", "rocket-raccoon"]
    let p = player1 tg
        other = player2 tg
        pair = ChooseCharacterWithTraitCost Avenger CharacterCanBeReadied
          <> ChooseCharacterWithTraitCost Guardian CharacterCanBeReadied
    inGame tg do
      changeForm p
      resolve [IdentityMessage other (ChangedToForm A)]
      resolve [IdentityMessage p ExhaustedIdentity]
      canAffordAllianceCost [p] GameRef pair `shouldReturn` False
      resolve [IdentityMessage other ExhaustedIdentity]
      canAffordAllianceCost [p] GameRef pair `shouldReturn` True
      canAffordAllianceCost [p] GameRef
        (ChooseCharacterWithTraitCost Avenger CharacterCanBeReadied <> ChooseCharacterWithTraitCost Avenger CharacterCanBeReadied)
        `shouldReturn` False
      acid <- getRandom
      withGame_ $ activeCostL .~ [ActiveCost acid p ForTreachery pair NoPayment Nothing [] Nothing]
      runNow [PayCost acid (ChooseCharacterWithTraitCost Avenger CharacterCanBeReadied)]
      runNow [PayCost acid (ChooseCharacterWithTraitCost Guardian CharacterCanBeReadied)]
      paymentChosenCharacters <$> getCurrentPayment `shouldReturn` [IdentityCharacter p, IdentityCharacter other]
      playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` True
      playerIdentityExhausted . toAttrs <$> getPlayer other `shouldReturn` True
