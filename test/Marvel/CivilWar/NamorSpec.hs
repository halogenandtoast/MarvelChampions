module Marvel.CivilWar.NamorSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Damage
import Marvel.Keyword qualified as K
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import TestImport

spec :: Spec
spec = describe "Civil War Namor" do
  it "Namor enters tough and stalwart and schemes with a villainous boost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary]
      mid <- addMinion you C.cw56160
      mainSchemeThreat `shouldReturn` 2
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem K.Stalwart)
      resolve [GiveStatus (MinionRef mid) ConfusedStatus]
      getCharacterStatusCount (MinionCharacter mid) ConfusedStatus `shouldReturn` 0

  it "Namor attacks the revealing hero with a villainous boost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary]
      _ <- addMinion you C.cw56160
      damageOf you `shouldReturn` 2

  for_ [C.cw56161,C.cw56162] $ \def -> do
    it (show (toCardCode def) <> " attaches to Namor in preference to the leader") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary]
        mid <- addMinion you C.cw56160
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter mid)
        modifiers <- getModifiers (MinionRef mid)
        modifiers `shouldSatisfy` elem (if toCardCode def == "56161" then AttackModifier 1 else SchemeModifier 1)

    it (show (toCardCode def) <> " attaches to the enemy leader without Namor") do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        card <- encounterCard def
        resolve [RevealEncounterCard (player1 tg) card]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)

  it "Neptune's Trident gives the leader's attacks piercing and one extra attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      card <- encounterCard C.cw56161
      resolve [RevealEncounterCard you card,GiveStatus (IdentityRef you) ToughStatus]
      changeForm you
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      resolve [VillainMessage vid $ VillainAttacks you]
      damageOf you `shouldReturn` 3
      getCharacterStatusCount (IdentityCharacter you) ToughStatus `shouldReturn` 0

  it "Horn of Proteus gives a scheming leader tough and one extra scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      card <- encounterCard C.cw56162
      resolve [RevealEncounterCard you card]
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      resolve [VillainMessage vid VillainSchemes]
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
      mainSchemeThreat `shouldReturn` 3

  it "Horn of Proteus gives a scheming Namor tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier,M.hydraMercenary]
      mid <- addMinion you C.cw56160
      resolve [RemoveStatus (MinionRef mid) ToughStatus]
      card <- encounterCard C.cw56162
      resolve [RevealEncounterCard you card,MinionMessage mid MinionSchemes]
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1
      mainSchemeThreat `shouldReturn` 5

  it "Imperius Rex activates the enemy leader when Namor is absent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- withLeader "57040"
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      card <- encounterCard C.cw56163
      resolve [RevealEncounterCard (player1 tg) card]
      mainSchemeThreat `shouldReturn` 2

  it "Imperius Rex activates Namor with a boost instead of the leader" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57040"
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary,M.hydraSoldier]
      _ <- addMinion you C.cw56160
      card <- encounterCard C.cw56163
      resolve [RevealEncounterCard you card]
      mainSchemeThreat `shouldReturn` 4

  it "Imperius Rex's boost deals and resolves an additional boost card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      [_,_,next] <- stackEncounterDeck [C.cw56163,M.hydraSoldier,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreat `shouldReturn` 3
      headMay <$> getEncounterDeck `shouldReturn` Just next

  it "Ruler of Atlantis protects Atlantis minions continuously and uses hinder" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you C.cw56165
      other <- addMinion you M.hydraSoldier
      card <- encounterCard C.cw56164
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 5
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem CannotTakeDamage)
      getModifiers (MinionRef other) >>= (`shouldSatisfy` notElem CannotTakeDamage)
      L.runQueueT (L.capture $ L.dealDamage mid you $ toDamage 2 FromAbility) >>= resolve
      minionDamageOf mid `shouldReturn` Just 0
      resolve [RemoveFromPlay $ SideSchemeRef sid]
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` notElem CannotTakeDamage)

  it "Ruler of Atlantis does not protect the other board's Atlantis minions" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- addSecondBoard tg
      mid <- addMinion (player2 tg) C.cw56165
      card <- encounterCard C.cw56164
      resolve [RevealEncounterCard you card]
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` notElem CannotTakeDamage)

  for_ [(C.cw56161,"01088"),(C.cw56162,"01089")] $ \(def,resource) ->
    it (show (toCardCode def) <> " is discarded by paying its two specified resources in hero form") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        clearHand you
        first <- putInHand you resource
        second <- putInHand you "01090"
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        offers (isAbilityOf $ attachmentIdOf card) `shouldReturn` False
        changeForm you
        choose "discard the equipment" (isAbilityOf $ attachmentIdOf card)
        payWith [first,second]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
        getEncounterDiscard >>= (`shouldSatisfy` elem card)

  it "Horn of Proteus does not give tough when confusion replaces a leader's scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      card <- encounterCard C.cw56162
      resolve [RevealEncounterCard you card,GiveStatus (VillainRef vid) ConfusedStatus]
      resolve [VillainMessage vid VillainSchemes]
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 0

  it "Imperius Rex's extra boost also resolves during Namor's villainous activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      [_,_,next] <- stackEncounterDeck [C.cw56163,M.hydraSoldier,M.hydraMercenary]
      _ <- addMinion (player1 tg) C.cw56160
      mainSchemeThreat `shouldReturn` 3
      headMay <$> getEncounterDeck `shouldReturn` Just next

  it "a Namor on the other board does not take the local leader's equipment" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      own <- villainIdNow
      _ <- addSecondBoard tg
      _ <- addMinion (player2 tg) C.cw56160
      card <- encounterCard C.cw56161
      resolve [RevealEncounterCard you card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter own)
