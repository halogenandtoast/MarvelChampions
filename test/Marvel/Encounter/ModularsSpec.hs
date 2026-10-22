-- | The Green Goblin and Once and Future Kang modular sets: the cards that ask something
-- new of the engine.
module Marvel.Encounter.ModularsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attrs (..))
import Marvel.Matchers (EnemyMatcher (AttackableBy), IdentityMatcher (IdentityWithId))
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types (getModifiedAttack)
import Marvel.Obligation.Cards qualified as ObligationCards
import Marvel.Query (selectList)
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Treachery.Cards qualified as TreacheryCards
import TestImport

spec :: Spec
spec = describe "modular sets" do
  it "Media Coverage resolves each When Revealed its player reveals once more, incite and surge too" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- revealNow you AttachmentCards.mediaCoverage
      before <- mainSchemeThreatNow
      dealtBefore <- length <$> dealtTo you
      _ <- revealNow you Cards.timeDisplacedSoldier
      after <- mainSchemeThreatNow
      dealtAfter <- length <$> dealtTo you
      liftIO $ (after - before, dealtAfter - dealtBefore) `shouldBe` (2, 2)

  it "Fear of Kang stays in the revealing player's play area: they cannot attack Kang, who grows with it" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- revealNow you ObligationCards.fearOfKang
      kept <- obligationsInPlay
      kang <- addMinion you Cards.kangMasterOfTime
      attackable <- selectList (AttackableBy (IdentityWithId you))
      atk <- maybe (pure 0) (getModifiedAttack . toAttrs) =<< getsGame (HashMap.lookup kang . gameMinions)
      liftIO do
        kept `shouldBe` 1
        EnemyMinionId kang `notElem` attackable `shouldBe` True
        atk `shouldBe` 2

  it "Goblin Glider attaches to the enemy with the most printed hit points without one" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      tombstone <- addMinion you Cards.tombstone
      _ <- revealNow you AttachmentCards.goblinGlider
      _ <- revealNow you AttachmentCards.goblinGlider
      attached <- getsGame (map (attachmentEnemy . toAttrs) . HashMap.elems . gameAttachments)
      let enemies = catMaybes attached
      liftIO $ (length enemies, all (`elem` enemies) [EnemyVillainId vid, EnemyMinionId tombstone]) `shouldBe` (2, True)

  it "Lightning Bolt deals indirect damage for each boost icon on the cards it discards" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      -- Whiplash has 2 boost icons, Hydra Mercenary 1
      _ <- stackEncounterDeck [Cards.whiplash, Cards.hydraMercenary]
      _ <- revealNow you TreacheryCards.lightningBolt
      damage <- damageOf you
      liftIO $ damage `shouldBe` 3

  it "Time Portal, defeated, is shuffled into the encounter deck" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      portal <- revealNow you SideSchemeCards.timePortal
      resolve [SideSchemeMessage (sideSchemeOf portal) (SideSchemeThwarted (toSource you) 99)]
      deck <- scenarioEncounterDeckNow
      discard <- encounterDiscardNow
      liftIO $ (portal `elem` deck, portal `elem` discard) `shouldBe` (True, False)
