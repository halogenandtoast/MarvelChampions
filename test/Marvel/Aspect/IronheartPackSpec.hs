module Marvel.Aspect.IronheartPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Ability (AbilityType (HeroInterrupt, Response, HeroResponse, ForcedInterrupt))
import Marvel.Ally.Types qualified as Ally
import Marvel.Attack qualified as Attack
import Marvel.Choice (choiceMessages)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Cost (canAffordCost)
import Marvel.Cost.Types
import Marvel.Keyword (Keyword (Guard, Overkill, Retaliate, Permanent, Patrol))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Trait (Trait (Aerial, Mercenary))
import TestImport

pack :: IO TestGame
pack = testGame ["ironheart"]

playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = traverse (putInHand p) codes >>= playWith card

ownResponse :: PlayerCard -> Choice -> Bool
ownResponse card = \case
  UseAbility ability -> abilityIndex ability == 1 && isAbilityOfCard card (UseAbility ability)
  _ -> False

playSnowguard :: IdentityId -> Natural -> GameAppT PlayerCard
playSnowguard p n = do
  card <- putInHand p "29023"
  playPaying p card ["01089", "01088"]
  choose "shift into a form" (ownResponse card)
  choose "choose shift counters" (isLabel $ countLabel "placeShiftCounters" n)
  pure card

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . playerIdentityDeck <$> playerAttrs p

hurt :: IdentityId -> CharacterId -> Natural -> GameAppT ()
hurt who character n = choiceMessages who (DamageCharacter character GameRef $ toDamage n FromAbility) >>= resolve

spec :: Spec
spec = describe "IronheartPack" do
  describe "Snowguard (29023)" do
    for_ [0..3] \n -> it ("chooses " <> show n <> " shift counters and gains exactly that form") do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- playSnowguard p n
        attrs <- allyIn "29023"
        allyCounters attrs `shouldBe` n
        Ally.getModifiedAttack attrs `shouldReturn` if n == 1 then 3 else 0
        Ally.getModifiedThwart attrs `shouldReturn` if n == 2 then 3 else 0
        Ally.getModifiedHitPoints attrs `shouldReturn` if n == 3 then 8 else 3
        modifiers <- getModifiers (allyIdOf card)
        (KeywordModifier Overkill `elem` modifiers) `shouldBe` (n == 1)
        (TraitModifier Aerial `elem` modifiers) `shouldBe` (n == 2)
        (KeywordModifier (Retaliate 1) `elem` modifiers) `shouldBe` (n == 3)

    it "can make a basic thwart with its gained THW despite printed zero THW" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- playSnowguard p 2
        scheme <- mainSchemeIdNow
        resolve [MainSchemeMessage scheme $ MainSchemePlaceThreat 4]
        attrs <- allyIn "29023"
        choose "Snowguard thwarts" (isAllyThwart attrs)
        choose "the main scheme" (isTargetChoice scheme)
        mainSchemeThreat `shouldReturn` 1
        map allyDamage <$> alliesWith "29023" `shouldReturn` [1]

    it "retaliates with the keyword gained from three counters" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- playSnowguard p 3
        villain <- villainIdNow
        let attack = Attack.attack (EnemyVillainId villain) (AllyCharacter $ allyIdOf card) 1
        resolve [AllyMessage (allyIdOf card) $ AllyWasAttacked attack]
        villainDamageNow `shouldReturn` 1

  describe "Vivian (29024)" do
    it "blanks a non-Elite minion until round end, preserving traits and status cards" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        guard <- addMinion p Minions.armoredGuard
        card <- putInHand p "29024"
        playPaying p card ["01089"]
        choose "Vivian response" (ownResponse card)
        choose "blank Armored Guard" (isTargetChoice guard)
        attrs <- getsGame $ toAttrs . fromMaybe (error "missing minion") . HashMap.lookup guard . gameMinions
        Minion.getModifiedKeywords attrs `shouldReturn` []
        minionTough attrs `shouldBe` True
        HashSet.member Mercenary (cdTraits $ getCardDef attrs) `shouldBe` True
        resolve [EndRound]
        Minion.getModifiedKeywords attrs >>= (`shouldSatisfy` elem Guard)

    it "does not trigger in alter-ego form or when only an Elite minion is available" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        _ <- addMinion p Minions.modok
        card <- putInHand p "29024"
        playPaying p card ["01089"]
        offers (ownResponse card) `shouldReturn` False

    it "blanks a side scheme's hazard icon, then restores it at round end" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        scheme <- revealNow p SideSchemes.breakinAndTakin
        card <- putInHand p "29024"
        playPaying p card ["01089"]
        choose "Vivian response" (ownResponse card)
        choose "blank the scheme" (isTargetChoice $ sideSchemeOf scheme)
        getHazardCount `shouldReturn` 0
        resolve [EndRound]
        getHazardCount `shouldReturn` 1

    it "blanks Cyborg Tech's text while retaining its printed attack modifier" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        minion <- addMinion p Minions.hydraMercenary
        attachment <- encounterCard Attachments.cyborgTech
        resolve [RevealEncounterCard p attachment]
        card <- putInHand p "29024"
        playPaying p card ["01089"]
        choose "Vivian response" (ownResponse card)
        choose "blank Cyborg Tech" (isTargetChoice $ AttachmentId $ unCardId $ ecCardId attachment)
        attrs <- getsGame $ toAttrs . fromMaybe (error "missing minion") . HashMap.lookup minion . gameMinions
        Minion.getModifiedAttack attrs `shouldReturn` 2
        Minion.getModifiedHitPoints attrs `shouldReturn` 3
        Minion.getModifiedKeywords attrs >>= (`shouldSatisfy` notElem (Retaliate 1))

  describe "Go for Champions! (29025)" do
    it "prevents Champion damage across the table and to later entrants, expires, and removes itself" do
      tg <- testGame ["ironheart", "nova"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        resolve [IdentityMessage other (ChangedToForm A)]
        card <- putInHand p "29025"
        playPaying p card ["01089", "01083"]
        hurt p (IdentityCharacter p) 2
        hurt other (IdentityCharacter other) 2
        damageOf p `shouldReturn` 0
        damageOf other `shouldReturn` 0
        champion <- putIntoPlay p "29023"
        normal <- putIntoPlay p "01083"
        hurt p (AllyCharacter $ allyIdOf champion) 1
        hurt p (AllyCharacter $ allyIdOf normal) 1
        map allyDamage <$> alliesWith "29023" `shouldReturn` [0]
        map allyDamage <$> alliesWith "01083" `shouldReturn` [1]
        discardOf p >>= (`shouldSatisfy` notElem card)
        deckOf p >>= (`shouldSatisfy` notElem card)
        canAffordCost p GameRef (DamageCost 1) `shouldReturn` False
        runNow [EndRound]
        hurt p (IdentityCharacter p) 1
        damageOf p `shouldReturn` 1

    it "requires the identity to have Champion and hero form" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "29025"
        _ <- putInHand p "01089"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

  describe "Ingenuity (29027)" do
    it "requires Genius when played and enforces max one per player" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putInHand p "29027"
        playPaying p card ["01089"]
        second <- putInHand p "29027"
        _ <- putInHand p "01089"
        offers (isPlay second) `shouldReturn` False

    it "exhausts to generate a mental resource in alter-ego form" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        ingenuity <- putInHand p "29027"
        playPaying p ingenuity ["01089"]
        support <- putInHand p "01092"
        payment <- putInHand p "01089"
        choose "play Helicarrier" (isPlay support)
        choose "generate mental" (isAbilityOfCard ingenuity)
        choose "pay the remainder" (isPayWith payment)
        chooseIf isFinishPayment
        map supportExhausted <$> supportsWith "01092" `shouldReturn` [False]
        map upgradeExhausted <$> upgradesWith "29027" `shouldReturn` [True]

    it "cannot be played once Riri has changed to her non-Genius hero form" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "29027"
        _ <- putInHand p "01089"
        offers (isPlay card) `shouldReturn` False

  describe "Bombshell (29033)" do
    it "divides among legal enemies evenly and excludes a guarded villain" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "29033"
        playPaying p card ["01089", "01088"]
        first <- addMinion p Minions.hydraMercenary
        second <- addMinion p Minions.shocker
        attrs <- allyIn "29033"
        choose "Bombshell attacks" (isAllyAttack attrs)
        choose "give the remainder to Shocker" (isTargetChoice second)
        villainDamageNow `shouldReturn` 0
        minionDamageOf first `shouldReturn` Just 1
        minionDamageOf second `shouldReturn` Just 2
        map allyDamage <$> alliesWith "29033" `shouldReturn` [1]

    it "applies Last Stand's ATK bonus to the whole divided attack and discards Bombshell afterward" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        bombshell <- putIntoPlay p "29033"
        first <- addMinion p Minions.hydraMercenary
        second <- addMinion p Minions.shocker
        lastStand <- putInHand p "15029"
        attrs <- allyIn "29033"
        choose "Bombshell attacks" (isAllyAttack attrs)
        choose "initial remainder" (isTargetChoice second)
        playWith lastStand []
        pass
        minionDamageOf first `shouldReturn` Nothing
        -- Each minion has 3 hit points; both receive a share of three.
        minionDamageOf second `shouldReturn` Nothing
        alliesWith "29033" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem bombshell)

  describe "Wasp (29034)" do
    it "can attack the villain through guard" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "29034"
        playPaying p card ["01089"]
        _ <- addMinion p Minions.hydraMercenary
        attrs <- allyIn "29034"
        choose "Wasp attacks" (isAllyAttack attrs)
        villain <- villainIdNow
        choose "attack the guarded villain" (isTargetChoice villain)
        villainDamageNow `shouldReturn` 1

    it "ignores patrol and crisis while thwarting" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "29034"
        playPaying p card ["01089"]
        scheme <- mainSchemeIdNow
        resolve [MainSchemeMessage scheme $ MainSchemePlaceThreat 4]
        crisis <- encounterCard SideSchemes.crowdControl
        resolve [RevealEncounterCard p crisis]
        minion <- addMinion p (Minions.hydraMercenary {cdKeywords = HashSet.singleton Patrol})
        attrs <- allyIn "29034"
        choose "Wasp thwarts" (isAllyThwart attrs)
        choose "the main scheme through patrol and crisis" (isTargetChoice scheme)
        mainSchemeThreat `shouldReturn` 2

  describe "Pinpoint (29035)" do
    it "shuffles another player's discarded card into its owner's deck and exhausts" do
      tg <- testGame ["ironheart", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        changeForm p
        card <- putInHand p "29035"
        playPaying p card ["01089"]
        upgrade <- putIntoPlay other "01093"
        runNow [RemoveFromPlay $ UpgradeRef $ upgradeIdOf upgrade]
        choose "Pinpoint replaces the discard" (ownResponse card)
        deckOf other >>= (`shouldSatisfy` elem upgrade)
        discardOf other >>= (`shouldSatisfy` notElem upgrade)
        map allyExhausted <$> alliesWith "29035" `shouldReturn` [True]

    it "can replace its own discard while it is still in play to pay its exhaust cost" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "29035"
        playPaying p card ["01089"]
        runNow [RemoveFromPlay $ AllyRef $ allyIdOf card]
        choose "save Pinpoint himself" (ownResponse card)
        deckOf p >>= (`shouldSatisfy` elem card)
        discardOf p >>= (`shouldSatisfy` notElem card)
        alliesWith "29035" `shouldReturn` []

    it "does not offer its interrupt for an encounter card or a discard from hand" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "29035"
        playPaying p card ["01089"]
        minion <- addMinion p Minions.hydraBomber
        runNow [RemoveFromPlay $ MinionRef minion]
        offers (ownResponse card) `shouldReturn` False
        hand <- putInHand p "01089"
        runNow [DiscardedCard $ PlayerCard hand]
        offers (ownResponse card) `shouldReturn` False

    it "does not offer Pinpoint after Collector replaces the pending discard" do
      tg <- testGameAgainst "16070" ["ironheart"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        pinpoint <- putIntoPlay p "29035"
        other <- putIntoPlay p "01084"
        runNow [RemoveFromPlay $ AllyRef $ allyIdOf other]
        choose "Collector intercepts the discard first" \case
          UseAbility a -> abilityType a == ForcedInterrupt
          _ -> False
        offers (ownResponse pinpoint) `shouldReturn` False
        map allyExhausted <$> alliesWith "29035" `shouldReturn` [False]
        cards <- getsGame gameCollection
        map toCardId cards `shouldSatisfy` elem (pcCardId other)
