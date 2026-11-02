module Marvel.CivilWar.IronManSpec (spec) where

import Marvel.Attachment.Types qualified as A
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,leaderHp,attachmentIdOf)
import Marvel.CivilWar.IronManAttachments (ironManDefinitions)
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Trait qualified as T
import TestImport

armorDamage :: AttachmentId -> GameAppT Natural
armorDamage aid = getsGame $ maybe 0 (A.attachmentDamage . toAttrs) . lookup aid . gameAttachments

spec :: Spec
spec = describe "Iron Man encounter attachments" do
  for_ [(C.cw56065,[AttackModifier 1,KeywordModifier K.Ranged],["01088","01090"])
       ,(C.cw56066,[AttackModifier 1,TraitModifier T.Aerial],["01088","01090"])
       ,(C.cw56067,[KeywordModifier K.Stalwart],["01089","01090"])
       ,(C.cw56068,[KeywordModifier $ K.Retaliate 1],["01089","01090"])] $ \(def,expected,cost) ->
    it ("applies and pays the basic-attack response to remove " <> show (toCardCode def)) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        vid <- withLeader "56059"
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        modifiers <- getModifiers (VillainRef vid)
        for_ expected $ \modifier -> liftIO $ modifiers `shouldSatisfy` elem modifier
        getEnemyScheme (EnemyVillainId vid) `shouldReturn` 2
        resources <- traverse (putInHand you) cost
        choose "make a basic attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
        choose "attack Iron Man" (isTargetChoice vid)
        choose "discard the Tech attachment" (isAbilityOf $ attachmentIdOf card)
        payWith resources
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
        getEnemyScheme (EnemyVillainId vid) `shouldReturn` 1

  it "caps Iron Man's Tech scheme bonus at three" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "56059"
      for_ ironManDefinitions $ \def -> do
        card <- encounterCard def
        resolve [RevealEncounterCard (player1 tg) card]
      getEnemyScheme (EnemyVillainId vid) `shouldReturn` 4

  it "absorbs every damage instance on Mark V Armor and discards it at five or more" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "56059"
      armor <- encounterCard C.cw56069
      resolve [RevealEncounterCard (player1 tg) armor]
      before <- leaderHp vid
      resolve [VillainMessage vid $ VillainDamaged GameRef (toDamage 3 FromAbility)]
      leaderHp vid `shouldReturn` before
      armorDamage (attachmentIdOf armor) `shouldReturn` 3
      resolve [VillainMessage vid $ VillainDamaged GameRef (toDamage 4 FromAbility)]
      leaderHp vid `shouldReturn` before
      getAttachedCharacter (attachmentIdOf armor) `shouldReturn` Nothing

  it "does not place damage on Mark V Armor when tough prevents it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "56059"
      armor <- encounterCard C.cw56069
      resolve [RevealEncounterCard (player1 tg) armor,GiveStatus (VillainRef vid) ToughStatus]
      before <- leaderHp vid
      resolve [VillainMessage vid $ VillainDamaged GameRef (toDamage 3 FromAbility)]
      leaderHp vid `shouldReturn` before
      armorDamage (attachmentIdOf armor) `shouldReturn` 0
      villainMatches VillainWithToughStatus vid `shouldReturn` False

  for_ [C.cw56065,C.cw56066,C.cw56069] $ \def ->
    it ("reveals " <> show (toCardCode def) <> " as a boost and attaches it to Iron Man") do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- withLeader "56059"
        card <- encounterCard def
        resolve [RevealBoostCard card (EnemyVillainId vid),ClearBoosts]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)

  it "lets the enemy team choose Iron Man's setup attachment" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "56059"
      _ <- stackEncounterDeck ironManDefinitions
      start [CardSpecific (VillainRef vid) "cwLeaderSetup" Null]
      choose "choose Mark V Helmet" $ \case CardLabel code _ -> code == "56067"; _ -> False
      pass
      getEnemyKeywords (EnemyVillainId vid) >>= (`shouldSatisfy` elem K.Stalwart)
      villainMatches VillainWithToughStatus vid `shouldReturn` True
