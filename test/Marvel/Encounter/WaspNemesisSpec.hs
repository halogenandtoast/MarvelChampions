module Marvel.Encounter.WaspNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card.Side (Side (A))
import Marvel.Hp (HP (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types (villainHp)
import TestImport

wasp :: IO TestGame
wasp = testGame ["wasp"]

-- | Wasp in Tiny form (ATK 1, DEF 2), her hand empty.
tiny :: IdentityId -> GameAppT ()
tiny p = do
  changeFormTo p A
  pass
  clearHand p

minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs m = getsGame (maybe (error "no such minion") toAttrs . HashMap.lookup m . gameMinions)

minionAttackOf, minionHitPointsOf :: MinionId -> GameAppT Natural
minionAttackOf m = Minion.getModifiedAttack =<< minionAttrs m
minionHitPointsOf m = Minion.getModifiedHitPoints =<< minionAttrs m

villainHpNow :: GameAppT Int
villainHpNow = unHp . villainHp . toAttrs <$> getVillain

encounterDeckCodes :: GameAppT [CardCode]
encounterDeckCodes = map toCardCode <$> scenarioEncounterDeckNow

-- | Beetle on 3 damage, and Wasp's basic attack finishing him.
attackBeetle :: IdentityId -> MinionId -> GameAppT ()
attackBeetle p m = do
  resolve [MinionMessage m (MinionDamaged GameRef (toDamage 3 FromAbility))]
  choose "attack" (isHeroAttack p)
  choose "Beetle" (isTargetChoice m)
  chooseIf isAnyAbility

spendPhysical, shuffleBeetle :: Choice -> Bool
spendPhysical = isLabel "$label.spendAPhysicalResource"
shuffleBeetle = isLabel "$label.shuffleBeetleIntoEncounterDeck"

spec :: Spec
spec = describe "Wasp nemesis" do
  describe "Mother's Orders (13027)" do
    it "a hero must spend 1 resource of any type to make a basic attack" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        tiny p
        _ <- revealNow p SideSchemes.mothersOrders
        pass
        offers (isHeroAttack p) `shouldReturn` False
        card <- putInHand p "01090"
        offers (isHeroAttack p) `shouldReturn` True
        choose "attack" (isHeroAttack p)
        choose "pay" (isPayWith card)
        chooseIf isFinishPayment
        chooseVillain
        villainDamageNow `shouldReturn` 1
        handOf p `shouldReturn` []

    it "costs nothing more once it is defeated" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        tiny p
        scheme <- sideSchemeOf <$> revealNow p SideSchemes.mothersOrders
        pass
        resolve [SideSchemeMessage scheme (SideSchemeThwarted GameRef 2)]
        offers (isHeroAttack p) `shouldReturn` True
        choose "attack" (isHeroAttack p)
        chooseVillain
        villainDamageNow `shouldReturn` 1

  describe "Beetle (13028)" do
    it "the player who defeats him may spend a [physical] resource: he is defeated" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        tiny p
        m <- addMinion p Minions.beetle
        pass
        card <- putInHand p "01090"
        attackBeetle p m
        offers shuffleBeetle `shouldReturn` True
        choose "spend" spendPhysical
        choose "pay" (isPayWith card)
        chooseIf isFinishPayment
        pass
        minionDamageOf m `shouldReturn` Nothing
        handOf p `shouldReturn` []
        encounterDiscardCodes >>= (`shouldSatisfy` elem "13028")
        encounterDeckCodes >>= (`shouldSatisfy` notElem "13028")

    it "or shuffle him into the encounter deck: not defeated, his attachments discarded" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        tiny p
        m <- addMinion p Minions.beetle
        pass
        _ <- revealNow p Attachments.beetleArmorMkIV
        pass
        -- 8 hit points with the armor: 4 here, then 3 and Wasp's 1
        resolve [MinionMessage m (MinionDamaged GameRef (toDamage 4 FromAbility))]
        _ <- putInHand p "01090"
        attackBeetle p m
        choose "shuffle" shuffleBeetle
        pass
        minionDamageOf m `shouldReturn` Nothing
        length <$> handOf p `shouldReturn` 1
        encounterDeckCodes >>= (`shouldSatisfy` elem "13028")
        discarded <- encounterDiscardCodes
        discarded `shouldSatisfy` notElem "13028"
        discarded `shouldSatisfy` elem "13029"
        -- not defeated: Small but Mighty does not answer
        villainDamageNow `shouldReturn` 0

    it "is shuffled in when the player has no [physical] resource to spend" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        tiny p
        m <- addMinion p Minions.beetle
        pass
        _ <- putInHand p "01089"
        attackBeetle p m
        offers spendPhysical `shouldReturn` False
        pass
        minionDamageOf m `shouldReturn` Nothing
        encounterDeckCodes >>= (`shouldSatisfy` elem "13028")
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "13028")

    it "the choice is the defeating player's, not the engaged one's" do
      tg <- testGame ["wasp", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        m <- addMinion p Minions.beetle
        pass
        _ <- putInHand other "01090"
        runNow [MinionMessage m (MinionDamaged (IdentityRef other) (toDamage 4 FromAbility))]
        chooseIf isAnyAbility
        fmap fst <$> asked `shouldReturn` Just other
        offers spendPhysical `shouldReturn` True

  describe "Beetle Armor MK IV (13029)" do
    it "attaches to Beetle, who gets +4 hit points" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        m <- addMinion p Minions.beetle
        pass
        hp <- villainHpNow
        _ <- revealNow p Attachments.beetleArmorMkIV
        pass
        minionHitPointsOf m `shouldReturn` 8
        villainHpNow `shouldReturn` hp

    it "attaches to the villain without Beetle, his hit points gained and lost with it" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        hp <- villainHpNow
        card <- revealNow p Attachments.beetleArmorMkIV
        pass
        villainHpNow `shouldReturn` (hp + 4)
        resolve [RemoveFromPlay (AttachmentRef $ AttachmentId $ unCardId $ ecCardId card)]
        villainHpNow `shouldReturn` hp

  describe "Beetle Mania (13030)" do
    it "gains surge against an alter-ego" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        m <- addMinion p Minions.beetle
        pass
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.beetleMania
        pass
        damageOf p `shouldReturn` 0
        minionDamageOf m `shouldReturn` Just 0
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)

    it "Beetle attacks the hero with +1 ATK for that attack, and there is no surge" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        tiny p
        m <- addMinion p Minions.beetle
        pass
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.beetleMania
        choose "no defender" isNoDefender
        pass
        damageOf p `shouldReturn` 2
        minionAttackOf m `shouldReturn` 1
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `notElem`) . map ecCardId)

    it "gains surge against a hero when Beetle is not in play" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        tiny p
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.beetleMania
        pass
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)

    it "gains surge when Beetle is stunned: his attack is not made (RR, Stunned)" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        tiny p
        m <- addMinion p Minions.beetle
        pass
        resolve [MinionMessage m (MinionStunned GameRef)]
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.beetleMania
        pass
        damageOf p `shouldReturn` 0
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)
