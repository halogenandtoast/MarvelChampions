{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Encounter.AgentsOfShieldBlackWidowSpec (spec) where

import Data.HashMap.Strict qualified as HM
import Marvel.Ability.Types qualified as Ability
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Message.Lifted qualified as L
import Marvel.Modifier
import Marvel.Query
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

lifted :: L.QueueT GameAppT () -> GameAppT ()
lifted body = resolve =<< L.runQueueT (L.capture body)

settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = error "Black Widow attack did not finish resolving"
  go n = pressWhenRevealed >> openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you,q) | notNull (choicesOf q) -> do
      let responses = [idx | (idx,UseAbility _) <- zip [0..] (choicesOf q)]
      answerQuestion you (fromMaybe 0 $ headMay responses)
      go (n-1)
    _ -> pure ()

prepare :: GameAppT IdentityId
prepare = do
  p <- selectJust FirstPlayer
  mids <- selectList AnyMinion
  resolve [RemoveFromPlay $ MinionRef mid | mid <- mids]
  changeForm p
  pass
  clearHand p
  pure p

attackWidow :: IdentityId -> GameAppT ()
attackWidow p = do
  vid <- villainIdNow
  choose "basic attack" (isHeroAttack p)
  choose "attack Black Widow" (isTargetChoice vid)
  settle

preparation :: IdentityId -> CardDef -> GameAppT ()
preparation _ def = void $ setEncounterDeck [def,Treacheries.advance,Treacheries.advance,Treacheries.advance] []

spec :: Spec
spec = describe "Agents of S.H.I.E.L.D. Black Widow preparations" do
  it "removes a threat and resolves the discarded Preparation before taking attack damage" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      preparation p Cards.aos50077
      before <- mainSchemeThreatNow
      attackWidow p
      villainDamageNow `shouldReturn` 2
      mainSchemeThreatNow `shouldReturn` before
      isConfused p `shouldReturn` False

  it "does not discard an encounter card when no main-scheme threat can be removed" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      lifted $ L.removeThreat p p 99 MainScheme
      preparation p Cards.aos50076
      before <- encounterDeckSize
      attackWidow p
      encounterDeckSize `shouldReturn` before
      villainDamageNow `shouldReturn` 2

  it "stun replaces the attack before Black Widow can trigger" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      lifted $ L.giveStatus p StunnedStatus
      preparation p Cards.aos50079
      before <- mainSchemeThreatNow
      choose "clear stun with basic attack" (isHeroAttack p)
      settle
      mainSchemeThreatNow `shouldReturn` before
      isStunned p `shouldReturn` False
      villainDamageNow `shouldReturn` 0

  it "Attacrobatics prevents all damage from the player attack in standard" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      preparation p Cards.aos50076
      attackWidow p
      villainDamageNow `shouldReturn` 0
      damageOf p `shouldReturn` 0

  it "Grappling Hook's preparation discards an event without attaching the Hook" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      event <- putInHand p "01005"
      preparation p Cards.aos50069
      attackWidow p
      map pcCardId <$> handOf p >>= (`shouldSatisfy` notElem (pcCardId event))
      selectAny (AttachmentIs Cards.aos50069) `shouldReturn` False

  it "Stun Net attaches to the attacker only after the attack and prevents future attacks" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      preparation p Cards.aos50071
      attackWidow p
      villainDamageNow `shouldReturn` 2
      net <- selectJust (AttachmentIs Cards.aos50071)
      getsGame (join . fmap (Attachment.attachmentIdentity . toAttrs) . HM.lookup net . gameAttachments) `shouldReturn` Just p
      getModifiers (IdentityRef p) >>= (`shouldSatisfy` elem CannotAttack)

  it "A.I.M. Commando enters after the attack; its quickstrike then attacks the hero" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      preparation p Cards.aos50072
      attackWidow p
      villainDamageNow `shouldReturn` 2
      selectListCount (MinionIs Cards.aos50072) `shouldReturn` 1
      damageOf p >>= (`shouldSatisfy` (>=2))

  it "A.I.M. Grunt redirects this attack to itself" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      preparation p Cards.aos50073
      attackWidow p
      villainDamageNow `shouldReturn` 0
      grunt <- selectJust (MinionIs Cards.aos50073)
      minionDamageOf grunt `shouldReturn` Just 2

  it "Dance of Death's preparation deals one damage to every controlled character" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      ally <- putIntoPlay p "01083"
      preparation p Cards.aos50078
      attackWidow p
      damageOf p `shouldReturn` 1
      getsGame (maybe 0 allyDamage . fmap toAttrs . HM.lookup (allyIdOf ally) . gameAllies) `shouldReturn` 1

  it "Widow's Bite stuns the attacking hero after damage resolves" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      preparation p Cards.aos50079
      attackWidow p
      villainDamageNow `shouldReturn` 2
      isStunned p `shouldReturn` True

  it "Night Vision Goggles attaches from its preparation and prevents a later attack on a card without a printed preparation" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      preparation p Cards.aos50070
      attackWidow p
      selectAny (AttachmentIs Cards.aos50070) `shouldReturn` True
      villainDamageNow `shouldReturn` 2
      lifted $ L.ready p
      preparation p Treacheries.advance
      attackWidow p
      villainDamageNow `shouldReturn` 2
      selectAny (AttachmentIs Cards.aos50070) `shouldReturn` False

  it "Automated Defenses grants its damage preparation only to cards without a printed preparation" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      scheme <- encounterCard Cards.aos50074
      resolve [RevealEncounterCard p scheme]
      preparation p Treacheries.advance
      attackWidow p
      damageOf p `shouldReturn` 1
      lifted $ L.ready p
      preparation p Cards.aos50077
      attackWidow p
      damageOf p `shouldReturn` 1

  it "Destroy Evidence grants Incite 1 to other encounter cards" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      scheme <- encounterCard Cards.aos50075
      resolve [RevealEncounterCard p scheme]
      before <- mainSchemeThreatNow
      bite <- encounterCard Cards.aos50079
      resolve [RevealEncounterCard p bite]
      mainSchemeThreatNow `shouldReturn` (before+1)
      isStunned p `shouldReturn` True
      damageOf p `shouldReturn` 1

  for_ [(Treacheries.advance,False),(Cards.aos50077,True)] $ \(def,prepared) ->
    it ("Gauntlet retaliates before its optional discard, which is available only without a resolved Preparation, prepared=" <> show prepared) do
      tg <- testGameAgainst "50064" ["spider-man"]
      inGame tg do
        p <- prepare
        gauntlet <- encounterCard Cards.aos50068
        resolve [RevealEncounterCard p gauntlet]
        preparation p def
        attackWidow p
        damageOf p `shouldReturn` 1
        selectAny (AttachmentIs Cards.aos50068) `shouldReturn` prepared

  it "two Gauntlets combine retaliate into one damage instance, fully prevented by one tough card" do
    tg <- testGameAgainst "50064" ["spider-man"]
    inGame tg do
      p <- prepare
      for_ [1..2 :: Int] $ \_ -> do
        gauntlet <- encounterCard Cards.aos50068
        resolve [RevealEncounterCard p gauntlet]
      lifted $ L.giveStatus p ToughStatus
      preparation p Treacheries.advance
      attackWidow p
      damageOf p `shouldReturn` 0
      getCharacterStatusCount (IdentityCharacter p) ToughStatus `shouldReturn` 0
