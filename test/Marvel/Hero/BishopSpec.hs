module Marvel.Hero.BishopSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Message.Lifted ()
import Marvel.Ability qualified as Ability
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

abilityOn :: PlayerCard -> Natural -> Choice -> Bool
abilityOn card n c = isAbilityOfCard card c && case c of
  UseAbility a -> abilityIndex a == n
  _ -> False

-- Mandatory damage responses resolve before the optional absorption window.
resolveMandatory :: GameAppT ()
resolveMandatory = do
  question <- asked
  case question >>= (headMay . filter forced . snd) of
    Just choice -> choose "mandatory response" (== choice) >> resolveMandatory
    Nothing -> pure ()
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse]
    _ -> False

spec :: Spec
spec = describe "Bishop" do
  it "starts with twelve hit points and recovers four" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 12
      handSizeOf p `shouldReturn` 6
      setDamage p 6
      choose "recover" \case UseAbility a -> Recover `elem` abilityChoices a; _ -> False
      damageOf p `shouldReturn` 2

  it "absorbs attack damage, discarding exactly that many and keeping only resource cards" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- stackDeck p ["45010", "45007", "01088", "45008", "45009"]
      minion <- addMinion p Minions.laufey
      runNow [MinionMessage minion $ MinionAttacks p]
      choose "take the attack" isNoDefender
      resolveMandatory
      choose "Energy Absorption" (isNamedAbility "$label.energyAbsorption")
      pass
      damageOf p `shouldReturn` 4
      cardsInHand <- map toCardCode <$> handOf p
      cardsInHand `shouldMatchList` ["45010", "01088"]
      cards <- discardOf p
      any ((== "45007") . toCardCode) cards `shouldBe` True
      any ((== "45009") . toCardCode) cards `shouldBe` False

  it "does not offer Energy Absorption after nonattack damage" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      start (damageIdentity p GameRef $ toDamage 2 FromAbility)
      offers (isNamedAbility "$label.energyAbsorption") `shouldReturn` False
      pass
      damageOf p `shouldReturn` 2

  it "Lucas recovers a Temporal card after changing to alter ego, not other cards" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      temporal <- putInDiscard p "45010"
      ordinary <- putInDiscard p "45007"
      runNow [IdentityMessage p $ ChangedToForm B]
      choose "Temporally Displaced" (isNamedAbility "$label.temporallyDisplaced")
      offers (isTargetChoice ordinary) `shouldReturn` False
      choose "Stored Energy" (isTargetChoice temporal)
      handOf p `shouldReturn` [temporal]

  describe "Malcolm and Randall" do
    for_ [("45002", "01090"), ("45003", "01088")] \(code, resource) ->
      it (show code <> " readies, heals from the matching printed icon, and limits its action per phase") do
        tg <- testGame ["bishop"]
        inGame tg do
          let p = player1 tg
          asHero p
          card <- putIntoPlay p code
          let aid = allyIdOf card
          resolve [AllyMessage aid ExhaustedAlly, AllyMessage aid $ AllyDamaged GameRef $ toDamage 1 FromAbility]
          paid <- putInHand p resource
          choose "resource ready action" (abilityOn card 1)
          choose "discard resource" (isTargetChoice paid)
          pass
          attrs <- allyIn code
          allyExhausted attrs `shouldBe` False
          allyDamage attrs `shouldBe` 0
          _ <- putInHand p resource
          resolve [AllyMessage aid ExhaustedAlly]
          offers (abilityOn card 1) `shouldReturn` False

  it "Malcolm cannot heal from a wild icon without a printed physical icon" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putIntoPlay p "45002"
      resolve [AllyMessage (allyIdOf card) ExhaustedAlly, AllyMessage (allyIdOf card) $ AllyDamaged GameRef $ toDamage 1 FromAbility]
      paid <- putInHand p "01072"
      choose "Malcolm" (abilityOn card 1)
      choose "wild resource" (isTargetChoice paid)
      pass
      allyDamage <$> allyIn "45002" `shouldReturn` 1

  it "Bishop's Rifle counts resource cards rather than resource icons" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      rifle <- putIntoPlay p "45004"
      _ <- putInHand p "45010"
      _ <- putInHand p "01088"
      _ <- putInHand p "45007"
      choose "fire rifle" (abilityOn rifle 1)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  it "Bishop's Uniform heals after Energy Absorption using the resources now in hand" do
    tg <- testGame ["bishop"]
    inGame tg do
      let p = player1 tg
      asHero p
      uniform <- putIntoPlay p "45005"
      _ <- putInHand p "01088"
      _ <- stackDeck p ["45010"]
      minion <- addMinion p Minions.hydraMercenary
      runNow [MinionMessage minion $ MinionAttacks p]
      choose "take attack" isNoDefender
      choose "Energy Absorption" (isNamedAbility "$label.energyAbsorption")
      choose "uniform" (abilityOn uniform 1)
      pass
      damageOf p `shouldReturn` 0
