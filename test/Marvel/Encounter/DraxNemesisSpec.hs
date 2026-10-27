module Marvel.Encounter.DraxNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types (Attrs (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

-- | The player in hero form, past the change's responses.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

-- | The enemy the attachment is attached to, Nothing once it has left play.
attachedEnemy :: EncounterCard -> GameAppT (Maybe (Maybe EnemyId))
attachedEnemy card = getsGame (fmap (attachmentEnemy . toAttrs) . HashMap.lookup (attachmentOf card) . gameAttachments)

-- | The villain attacks the player, who does not defend; its boost card has no icons.
villainAttacksUndefended :: IdentityId -> GameAppT ()
villainAttacksUndefended p = do
  _ <- stackEncounterDeck [Treacheries.hailHydra]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]
  pass

-- | The minion attacks the player, who does not defend.
minionAttacksUndefended :: IdentityId -> MinionId -> GameAppT ()
minionAttacksUndefended p m = do
  _ <- stackEncounterDeck [Treacheries.hailHydra]
  runNow [MinionMessage m (MinionAttacks p)]
  pass

-- | Drax's basic attack against the enemy, his hand emptied so no Knife Leap interrupts.
draxAttacks :: (IsRef a) => IdentityId -> a -> GameAppT ()
draxAttacks p enemy = do
  clearHand p
  choose "attack" (isHeroAttack p)
  pass
  choose "the target" (isTargetChoice enemy)
  pass

-- | Stack a Hydra Mercenary on the encounter deck, reveal the card: the card, and whether
-- it surged, dealing the player the Mercenary facedown.
revealSurging :: IdentityId -> CardDef -> GameAppT (EncounterCard, Bool)
revealSurging p def = do
  [next] <- stackEncounterDeck [Minions.hydraMercenary]
  card <- revealNow p def
  pass
  surged <- elem (ecCardId next) . map ecCardId <$> dealtTo p
  pure (card, surged)

spec :: Spec
spec = describe "Drax's nemesis set" do
  describe "Cull the Weak (19026)" do
    it "has 2 threat per player and gives each enemy +2 ATK" do
      tg <- testGame ["drax", "spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        yotat <- addMinion p Minions.yotatTheDestroyer
        scheme <- revealNow p SideSchemes.cullTheWeak
        pass
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4
        villainAttacksUndefended p
        damageOf p `shouldReturn` 4
        minionAttacksUndefended p yotat
        damageOf p `shouldReturn` 9

  describe "Yotat the Destroyer (19027)" do
    it "has guard: the villain cannot be attacked while he is engaged" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        yotat <- addMinion p Minions.yotatTheDestroyer
        vid <- villainIdNow
        clearHand p
        choose "attack" (isHeroAttack p)
        pass
        offers (isTargetChoice yotat) `shouldReturn` True
        offers (isTargetChoice vid) `shouldReturn` False
        choose "attack Yotat" (isTargetChoice yotat)
        pass
        minionDamageOf yotat `shouldReturn` Just 1

    it "retaliates 1 when Drax attacks him" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        yotat <- addMinion p Minions.yotatTheDestroyer
        draxAttacks p yotat
        damageOf p `shouldReturn` 1

    it "attacks for 3" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        yotat <- addMinion p Minions.yotatTheDestroyer
        minionAttacksUndefended p yotat
        damageOf p `shouldReturn` 3

  describe "Challenge Accepted (19028)" do
    it "attaches to the enemy with the highest ATK, gives it +2 ATK, and surges" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        yotat <- addMinion p Minions.yotatTheDestroyer
        (card, surged) <- revealSurging p Attachments.challengeAccepted
        surged `shouldBe` True
        attachedEnemy card `shouldReturn` Just (Just (EnemyMinionId yotat))
        minionAttacksUndefended p yotat
        damageOf p `shouldReturn` 5

    it "the player chooses between enemies tied for the highest ATK" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        warrior <- addMinion p Minions.ancientWarrior
        vid <- villainIdNow
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        card <- revealNow p Attachments.challengeAccepted
        offers (isTargetChoice vid) `shouldReturn` True
        chooseIf (isTargetChoice warrior)
        pass
        attachedEnemy card `shouldReturn` Just (Just (EnemyMinionId warrior))

    it "is discarded after Drax deals 4 damage to the attached villain with one attack, not 1" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        vid <- villainIdNow
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        card <- revealNow p Attachments.challengeAccepted
        pass
        attachedEnemy card `shouldReturn` Just (Just (EnemyVillainId vid))
        -- ATK 1: not enough
        draxAttacks p vid
        attachedEnemy card `shouldReturn` Just (Just (EnemyVillainId vid))
        -- 3 vengeance counters: ATK 4
        runNow [IdentityMessage p ReadiedIdentity, PlaceCounters (IdentityRef p) 3]
        draxAttacks p vid
        attachedEnemy card `shouldReturn` Nothing
        encounterDiscardCodes >>= (`shouldSatisfy` elem "19028")

    it "is discarded by 4 or more damage Drax's attack deals it through overkill" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        vid <- villainIdNow
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        card <- revealNow p Attachments.challengeAccepted
        pass
        attachedEnemy card `shouldReturn` Just (Just (EnemyVillainId vid))
        regular <- addMinion p Minions.hydraRegular
        damage <- villainDamageNow
        -- 3 vengeance counters: ATK 4, and Knife Leap costs nothing
        runNow [PlaceCounters (IdentityRef p) 3]
        leap <- putInHand p "19005"
        choose "attack" (isHeroAttack p)
        choose "Knife Leap" (isPlay leap)
        payWith []
        pass
        choose "the Hydra Regular" (isTargetChoice regular)
        pass
        -- 9 to a 2-hit-point minion: 7 past it to the villain
        attachedEnemy card `shouldReturn` Nothing
        villainDamageNow `shouldReturn` (damage + 7)

  describe "\"I Will Destroy You!\" (19029)" do
    it "Hero: Yotat attacks you with +1 ATK, once; the villain does not attack" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        yotat <- addMinion p Minions.yotatTheDestroyer
        _ <- stackEncounterDeck [Treacheries.hailHydra]
        _ <- revealNow p Treacheries.iWillDestroyYou
        pass
        damageOf p `shouldReturn` 4
        -- his next attack has no bonus
        minionAttacksUndefended p yotat
        damageOf p `shouldReturn` 7

    it "Hero: with no Yotat in play, the villain attacks you" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- stackEncounterDeck [Treacheries.hailHydra]
        _ <- revealNow p Treacheries.iWillDestroyYou
        pass
        damageOf p `shouldReturn` 2

    it "Hero: a stunned Yotat only discards his stun, and the villain attacks you" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        yotat <- addMinion p Minions.yotatTheDestroyer
        resolve [MinionMessage yotat (MinionStunned GameRef)]
        _ <- stackEncounterDeck [Treacheries.hailHydra]
        _ <- revealNow p Treacheries.iWillDestroyYou
        pass
        damageOf p `shouldReturn` 2
        -- the stun is gone: he attacks for 3 now
        minionAttacksUndefended p yotat
        damageOf p `shouldReturn` 5

    it "Alter-Ego: gains surge, and nothing attacks" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.yotatTheDestroyer
        snd <$> revealSurging p Treacheries.iWillDestroyYou `shouldReturn` True
        damageOf p `shouldReturn` 0
