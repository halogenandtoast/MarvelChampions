module Marvel.Encounter.DoctorStrangeNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Hero.Heroes.DoctorStrange (InvocationDeck (..), invocationDeckOf)
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.SideScheme.Types (Attrs (sideSchemeHeldCards))
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

doctorStrange :: IO TestGame
doctorStrange = testGame ["doctor-strange"]

invocationOf :: IdentityId -> GameAppT InvocationDeck
invocationOf p = fromMaybe (error "no Invocation deck") . invocationDeckOf <$> getPlayer p

isPayWithCode :: CardCode -> Choice -> Bool
isPayWithCode code = \case
  PayWithCard c -> toCardCode c == code
  _ -> False

spec :: Spec
spec = describe "Doctor Strange's nemesis set" do
  describe "Baron Mordo" do
    it "discards the top card of your deck as he attacks you: [mental] confuses you" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        changeForm p
        mordo <- addMinion p Minions.baronMordo
        -- Genius is mental
        _ <- stackDeck p ["09023"]
        start [MinionMessage mordo (MinionAttacks p)]
        chooseIf isAnyAbility
        map toCardCode . take 1 <$> discardOf p `shouldReturn` ["09023"]
        isConfused p `shouldReturn` True
        isStunned p `shouldReturn` False

    it "a wild card stuns, confuses and deals 2 damage" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        changeForm p
        mordo <- addMinion p Minions.baronMordo
        _ <- stackDeck p ["09002"]
        start [MinionMessage mordo (MinionAttacks p)]
        chooseIf isAnyAbility
        isConfused p `shouldReturn` True
        isStunned p `shouldReturn` True
        resolve []
        damageOf p >>= (`shouldSatisfy` (>= 2))

  it "Open the Dark Dimension holds the top Invocation card until it is defeated" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      changeForm p
      card <- revealNow p SideSchemes.openTheDarkDimension
      resolve []
      let sid = sideSchemeOf card
      sideSchemeThreat sid `shouldReturn` 3
      length . (.invocationDeck) <$> invocationOf p `shouldReturn` 4
      held <- getsGame (maybe [] (sideSchemeHeldCards . toAttrs) . HashMap.lookup sid . gameSideSchemes)
      length held `shouldBe` 1
      resolve [SideSchemeMessage sid (SideSchemeThwarted (toSource p) 3)]
      length . (.invocationDeck) <$> invocationOf p `shouldReturn` 5

  describe "Counterspell" do
    it "attaches to your hero and cancels the next event you play, then is discarded" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        _ <- revealNow p Attachments.counterspell
        resolve []
        getsGame (HashMap.size . gameAttachments) `shouldReturn` 1
        haymaker <- putInHand p "01087"
        _ <- putInHand p "01088"
        playWith haymaker []
        chooseIf (isPayWithCode "01088")
        chooseIf isFinishPayment
        chooseIf isAnyAbility
        resolve []
        villainDamageNow `shouldReturn` 0
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "01087")
        getsGame (HashMap.null . gameAttachments) `shouldReturn` True

    it "is discarded as it comes, an alter-ego having no hero to attach to" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        card <- revealNow p Attachments.counterspell
        resolve []
        getsGame (HashMap.null . gameAttachments) `shouldReturn` True
        encounterDiscardNow >>= (`shouldSatisfy` elem card)

  describe "Thoughtcasting" do
    it "in hero form: discard the highest-cost card in hand and take its printed cost as damage" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        _ <- putInHand p "09036"
        blast <- putInHand p "09004"
        _ <- revealNow p Treacheries.thoughtcasting
        choose "Magic Blast" (isTargetChoice blast)
        resolve []
        damageOf p `shouldReturn` 3
        map toCardCode <$> handOf p `shouldReturn` ["09036"]

    it "in alter-ego form: places threat on the main scheme instead" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        clearHand p
        threat <- mainSchemeThreatNow
        studies <- putInHand p "09006"
        _ <- revealNow p Treacheries.thoughtcasting
        choose "Mystical Studies" (isTargetChoice studies)
        resolve []
        mainSchemeThreatNow `shouldReturn` (threat + 1)
        damageOf p `shouldReturn` 0
