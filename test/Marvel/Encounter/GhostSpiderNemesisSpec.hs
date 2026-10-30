module Marvel.Encounter.GhostSpiderNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Runner qualified as Minion
import Marvel.Phase (Phase (PlayerPhase, VillainPhase))
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait (Trait (Creature))
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

attachmentTarget :: CardCode -> GameAppT (Maybe EnemyId)
attachmentTarget code = getsGame $ headMay . mapMaybe (Attachment.attachmentEnemy . toAttrs)
  . filter ((== code) . toCardCode . getCardDef) . HashMap.elems . gameAttachments

spec :: Spec
spec = describe "Ghost-Spider nemesis" do
  describe "Regenerative Research and The Lizard" do
    it "heals each enemy at the start of the villain phase, with The Lizard healing one additional damage" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        lizard <- addMinion p Minions.theLizard
        soldier <- addMinion p Minions.hydraSoldier
        _ <- revealNow p Schemes.regenerativeResearch
        pass
        vid <- villainIdNow
        resolve [MinionMessage lizard (MinionDamaged GameRef $ toDamage 3 FromAbility)
          , MinionMessage soldier (MinionDamaged GameRef $ toDamage 2 FromAbility)
          , VillainMessage vid (VillainDamaged GameRef $ toDamage 3 FromAbility)]
        resolve [BeginPhase VillainPhase]
        minionDamageOf lizard `shouldReturn` Just 1
        minionDamageOf soldier `shouldReturn` Just 1
        villainDamageNow `shouldReturn` 2

    it "does not heal at the start of the player phase or above maximum hit points" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        lizard <- addMinion p Minions.theLizard
        resolve [MinionMessage lizard (MinionDamaged GameRef $ toDamage 1 FromAbility)]
        resolve [BeginPhase PlayerPhase]
        minionDamageOf lizard `shouldReturn` Just 1
        resolve [BeginPhase VillainPhase, BeginPhase VillainPhase]
        minionDamageOf lizard `shouldReturn` Just 0

  describe "Experimental Injection" do
    it "attaches to the minion with most remaining hit points and gives Creature and four hit points" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        lizard <- addMinion p Minions.theLizard
        soldier <- addMinion p Minions.hydraSoldier
        resolve [MinionMessage lizard (MinionDamaged GameRef $ toDamage 4 FromAbility)]
        _ <- revealNow p Attachments.experimentalInjection
        pass
        attachmentTarget "27028" `shouldReturn` Just (EnemyMinionId soldier)
        minions <- getsGame gameMinions
        Minion.getModifiedHitPoints (toAttrs $ fromMaybe (error "missing soldier") $ HashMap.lookup soldier minions) `shouldReturn` 8
        minionMatches (MinionWithTrait Creature) soldier `shouldReturn` True
        minionMatches (MinionWithTrait Creature) lizard `shouldReturn` True

    it "lets the revealing player choose among tied minions" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        first <- addMinion p Minions.hydraSoldier
        second <- addMinion p Minions.hydraSoldier
        _ <- revealNow p Attachments.experimentalInjection
        offers (isTargetChoice first) `shouldReturn` True
        offers (isTargetChoice second) `shouldReturn` True
        choose "choose the second minion" (isTargetChoice second)
        pass
        attachmentTarget "27028" `shouldReturn` Just (EnemyMinionId second)

    it "surges and is discarded when there is no minion" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Schemes.bombScare, Minions.hydraMercenary] []
        _ <- revealNow p Attachments.experimentalInjection
        pass
        attachmentTarget "27028" `shouldReturn` Nothing
        encounterDiscardCodes >>= (`shouldSatisfy` elem "27028")
        map toCardCode . playerIdentityEncounterCards <$> playerAttrs p >>= (`shouldSatisfy` elem "01109")

  describe "In Cold Blood" do
    it "makes The Lizard attack and blocks events during that attack, then restores event play" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        lizard <- addMinion p Minions.theLizard
        binding <- putInHand p "27006"
        jump <- putInHand p "27014"
        _ <- putInHand p "01089"
        _ <- putInHand p "01088"
        _ <- revealNow p Treacheries.inColdBlood
        offers (isPlay binding) `shouldReturn` False
        offers (isPlay jump) `shouldReturn` False
        choose "take the attack undefended" isNoDefender
        pass
        damageOf p `shouldReturn` 3
        start [MinionMessage lizard (MinionAttacks p)]
        offers (isPlay binding) `shouldReturn` True
        pass

    it "surges if The Lizard is absent" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Schemes.bombScare, Minions.hydraMercenary] []
        _ <- revealNow p Treacheries.inColdBlood
        pass
        damageOf p `shouldReturn` 0
        map toCardCode . playerIdentityEncounterCards <$> playerAttrs p >>= (`shouldSatisfy` elem "01109")

    it "surges when a stunned Lizard makes no attack and does not leave events blocked" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        clearHand p
        lizard <- addMinion p Minions.theLizard
        resolve [MinionMessage lizard (MinionStunned GameRef)]
        guidance <- putInHand p "27003"
        _ <- setEncounterDeck [Schemes.bombScare, Minions.hydraMercenary] []
        _ <- revealNow p Treacheries.inColdBlood
        pass
        damageOf p `shouldReturn` 0
        refreshTurn
        offers (isPlay guidance) `shouldReturn` True
        map toCardCode . playerIdentityEncounterCards <$> playerAttrs p >>= (`shouldSatisfy` elem "01109")

    it "attacks the revealing player even in alter-ego form" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.theLizard
        _ <- revealNow p Treacheries.inColdBlood
        pass
        damageOf p `shouldReturn` 3

    it "allows another player to cancel The Lizard's activation, then surges because no attack was made" do
      tg <- testGame ["ghost-spider", "spider-man-miles-morales"]
      let gwen = player1 tg
          miles = player2 tg
      inGame tg do
        changeForm gwen
        pass
        clearHand gwen
        clearHand miles
        _ <- addMinion miles Minions.theLizard
        binding <- putInHand gwen "27006"
        resource <- putInHand gwen "01089"
        _ <- setEncounterDeck [Schemes.bombScare, Minions.hydraMercenary] []
        _ <- revealNow miles Treacheries.inColdBlood
        playWith binding [resource]
        pass
        damageOf miles `shouldReturn` 0
        map toCardCode . playerIdentityEncounterCards <$> playerAttrs miles >>= (`shouldSatisfy` elem "01109")
