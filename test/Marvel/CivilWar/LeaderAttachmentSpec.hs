module Marvel.CivilWar.LeaderAttachmentSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf)
import Marvel.Keyword qualified as K
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import TestImport

spec :: Spec
spec = describe "Leader encounter equipment" do
  for_ [(C.cw56099,"56092"),(C.cw56144,"56137")] $ \(def,leader) ->
    it ("removes " <> show (toCardCode def) <> " after paying three resources of the same type") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        vid <- withLeader leader
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (SchemeModifier 1))
        getEnemyKeywords (EnemyVillainId vid) >>= (`shouldSatisfy` elem K.Stalwart)
        resources <- traverse (putInHand you) ["01088","01088","01088"]
        choose "make a basic attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
        choose "attack the leader" (isTargetChoice vid)
        choose "discard the helmet" (isAbilityOf $ attachmentIdOf card)
        payWith resources
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  for_ [(C.cw57047,["01088","01090"]),(C.cw57048,["01088","01089"])] $ \(def,cost) ->
    it ("discards " <> show (toCardCode def) <> " and flips the existing mass form") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        vid <- withLeader "57040"
        form <- encounterCard C.cw57046a
        card <- encounterCard def
        resolve [RevealEncounterCard you form,RevealEncounterCard you card]
        resources <- traverse (putInHand you) cost
        choose "make a basic attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
        choose "attack Vision" (isTargetChoice vid)
        choose "discard Vision's equipment" (isAbilityOf $ attachmentIdOf card)
        payWith resources
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
        getAttachedCharacter (attachmentIdOf form) `shouldReturn` Just (VillainCharacter vid)
        code <- getsGame $ fmap (toCardCode . getCardDef) . lookup (attachmentIdOf form) . gameAttachments
        liftIO $ code `shouldBe` Just "57046b"

  it "uses Super-Soldier Serum for the entire attack, including overkill" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56137"
      serum <- encounterCard C.cw56145
      resolve [RevealEncounterCard you serum]
      ally <- putIntoPlay you "01084"
      _ <- stackEncounterDeck [M.hydraMercenary]
      start [VillainMessage vid $ VillainAttacks you]
      chooseIf (isLabel "$label.noResponse")
      choose "grant overkill" (isAbilityOf $ attachmentIdOf serum)
      choose "defend with Nick Fury" $ \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
      pass
      damageOf you `shouldReturn` 2
      getAttachedCharacter (attachmentIdOf serum) `shouldReturn` Nothing

  it "discards Focused Rage after its increased scheme activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57001"
      rage <- encounterCard C.cw57008
      resolve [RevealEncounterCard (player1 tg) rage]
      getEnemyScheme (EnemyVillainId vid) `shouldReturn` 2
      _ <- stackEncounterDeck [M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      getAttachedCharacter (attachmentIdOf rage) `shouldReturn` Nothing
      getEnemyScheme (EnemyVillainId vid) `shouldReturn` 1

  it "switches Intangible to Dense before an attack and discards Density Control" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "57040"
      form <- encounterCard C.cw57046b
      density <- encounterCard C.cw57050
      resolve [RevealEncounterCard you form]
      resolve [PutEncounterCardIntoPlay you density]
      _ <- stackEncounterDeck [M.hydraMercenary]
      start [VillainMessage vid $ VillainAttacks you]
      choose "flip to Dense" (isAbilityOf $ attachmentIdOf density)
      pass
      getAttachedCharacter (attachmentIdOf density) `shouldReturn` Nothing
      code <- getsGame $ fmap (toCardCode . getCardDef) . lookup (attachmentIdOf form) . gameAttachments
      liftIO $ code `shouldBe` Just "57046a"

  for_ [(C.cw56099,"56092"),(C.cw56144,"56137"),(C.cw56145,"56137"),(C.cw57008,"57001"),(C.cw57047,"57040"),(C.cw57048,"57040")] $ \(def,leader) ->
    it ("reveals the boost on " <> show (toCardCode def)) do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- withLeader leader
        card <- encounterCard def
        resolve [RevealBoostCard card (EnemyVillainId vid),ClearBoosts]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)

  it "switches Dense to Intangible before scheming and discards Density Control" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      form <- encounterCard C.cw57046a
      density <- encounterCard C.cw57050
      resolve [RevealEncounterCard you form,PutEncounterCardIntoPlay you density]
      _ <- stackEncounterDeck [M.hydraMercenary]
      resolve [VillainMessage vid $ VillainSchemesAgainst you]
      getAttachedCharacter (attachmentIdOf density) `shouldReturn` Nothing
      code <- getsGame $ fmap (toCardCode . getCardDef) . lookup (attachmentIdOf form) . gameAttachments
      liftIO $ code `shouldBe` Just "57046b"

  it "keeps Density Control when the activation already uses the requested form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      form <- encounterCard C.cw57046b
      density <- encounterCard C.cw57050
      resolve [RevealEncounterCard you form,PutEncounterCardIntoPlay you density]
      _ <- stackEncounterDeck [M.hydraMercenary]
      resolve [VillainMessage vid $ VillainSchemesAgainst you]
      getAttachedCharacter (attachmentIdOf density) `shouldReturn` Just (VillainCharacter vid)
