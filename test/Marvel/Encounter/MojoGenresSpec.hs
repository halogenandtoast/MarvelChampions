module Marvel.Encounter.MojoGenresSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Environment.Cards qualified as Environments
import Marvel.Keyword
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Runner qualified as Minion
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

environmentOf :: EncounterCard -> EnvironmentId
environmentOf = EnvironmentId . unCardId . ecCardId

obligationOf :: EncounterCard -> ObligationId
obligationOf = ObligationId . unCardId . ecCardId

spec :: Spec
spec = describe "Mojo Mania genres" do
  describe "show environments" do
    for_ [(Environments.mojo39035, ThwartModifier 1), (Environments.mojo39041, HandSizeModifier 1), (Environments.mojo39066, DamageTakenModifier 1)] $ \(def, modifier) ->
      it ("applies " <> show modifier <> " while " <> show (toCardCode def) <> " is showing") do
        tg <- testGame ["spider-man"]
        inGame tg do
          card <- encounterCard def
          resolve [PutEncounterCardIntoPlay (player1 tg) card]
          getModifiers (player1 tg) >>= (`shouldSatisfy` elem modifier)
    it "revealing a new setting replaces the previous show" do
      tg <- testGame ["spider-man"]
      inGame tg do
        old <- revealNow (player1 tg) Environments.mojo39041
        pass
        _ <- revealNow (player1 tg) Environments.mojo39053
        pass
        selectList (EnvironmentIs Environments.mojo39041) `shouldReturn` []
        selectList (EnvironmentIs Environments.mojo39053) >>= (`shouldSatisfy` not . null)
        encounterDiscard >>= (`shouldSatisfy` elem old)
    it "The Mojo Files gives minions quickstrike" do
      tg <- testGame ["spider-man"]
      inGame tg do
        env <- encounterCard Environments.mojo39047
        resolve [PutEncounterCardIntoPlay (player1 tg) env]
        mid <- addMinion (player1 tg) Minions.hydraSoldier
        a <- getsGame (toAttrs . fromMaybe (error "minion missing") . HashMap.lookup mid . gameMinions)
        Minion.getModifiedKeywords a >>= (`shouldSatisfy` elem Quickstrike)
    it "Mojo Runner gives a newly entering minion a tough status card" do
      tg <- testGame ["spider-man"]
      inGame tg do
        env <- encounterCard Environments.mojo39053
        resolve [PutEncounterCardIntoPlay (player1 tg) env]
        mid <- addMinion (player1 tg) Minions.hydraSoldier
        getsGame (fmap (minionTough . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just True
    it "Mojo in the Middle adds acceleration to obligations" do
      tg <- testGame ["spider-man"]
      inGame tg do
        env <- encounterCard Environments.mojo39060
        resolve [PutEncounterCardIntoPlay (player1 tg) env]
        o <- revealNow (player1 tg) Obligations.mojo39036
        pass
        getModifiers (ObligationRef $ obligationOf o) >>= (`shouldSatisfy` elem (AccelerationIconModifier 1))

  describe "Crime" do
    it "Build the Case remains until three side schemes are defeated" do
      tg <- testGame ["spider-man"]
      inGame tg do
        o <- revealNow (player1 tg) Obligations.mojo39036
        pass
        for_ [1 :: Int, 2] $ \_ -> do
          s <- revealNow (player1 tg) Schemes.bombScare
          pass
          n <- sideSchemeThreat (sideSchemeOf s)
          resolve [SideSchemeMessage (sideSchemeOf s) (SideSchemeThwarted (IdentityRef $ player1 tg) n)]
        obligationIsInPlay (obligationOf o) `shouldReturn` True
        s <- revealNow (player1 tg) Schemes.bombScare
        pass
        n <- sideSchemeThreat (sideSchemeOf s)
        resolve [SideSchemeMessage (sideSchemeOf s) (SideSchemeThwarted (IdentityRef $ player1 tg) n)]
        obligationIsInPlay (obligationOf o) `shouldReturn` False
    it "Crime Scene Investigation blocks threat removal from other schemes" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Schemes.mojo39037
        pass
        main <- mainSchemeIdNow
        getModifiers main >>= (`shouldSatisfy` elem ThreatCannotBeRemoved)
    it "Law & Order reduces friendly ATK by two" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Schemes.mojo39038
        pass
        getModifiers (player1 tg) >>= (`shouldSatisfy` elem (AttackPenalty 2))
    it "Dragnet prevents the villain from taking damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Schemes.mojo39039
        pass
        vid <- villainIdNow
        getModifiers vid >>= (`shouldSatisfy` elem CannotTakeDamage)
    it "Elementary can discard into and reveal a side scheme" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Treacheries.advance, Schemes.bombScare, Minions.hydraSoldier] []
        _ <- revealNow (player1 tg) Treacheries.mojo39040
        choose "reveal a side scheme" (isLabel "$label.mojoRevealSideScheme")
        pass
        selectList (SideSchemeIs Schemes.bombScare) >>= (`shouldSatisfy` not . null)

  describe "Fantasy" do
    it "Dragon rewards every player with four cards when defeated" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mojo39042
        before <- length <$> handOf (player1 tg)
        other <- length <$> handOf (player2 tg)
        resolve [MinionMessage mid MinionDefeated]
        length <$> handOf (player1 tg) `shouldReturn` before + 4
        length <$> handOf (player2 tg) `shouldReturn` other + 4
    it "Goblin removes two threat from the chosen scheme when defeated" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Minions.hydraSoldier, Minions.hydraSoldier] []
        mid <- addMinion (player1 tg) Minions.mojo39043
        placeMainThreat 4
        before <- mainSchemeThreatNow
        resolve [MinionMessage mid MinionDefeated]
        mainSchemeThreatNow `shouldReturn` before - 2
    it "Troll can return a discarded ally to play when defeated" do
      tg <- testGame ["spider-man"]
      inGame tg do
        ally <- putInDiscard (player1 tg) "01059"
        mid <- addMinion (player1 tg) Minions.mojo39044
        runNow [MinionMessage mid MinionDefeated]
        choose "return ally" (isTargetChoice ally)
        alliesWith "01059" >>= (`shouldSatisfy` not . null)
    it "Fetch Quest lets its player play a searched card without resources" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Minions.hydraSoldier, Minions.hydraSoldier] []
        s <- revealNow (player1 tg) Schemes.mojo39045
        pass
        card <- putInHand (player1 tg) "01091"
        resolve [IdentityMessage (player1 tg) (ShuffleIntoIdentityDeck [card])]
        clearHand (player1 tg)
        runNow [SideSchemeMessage (sideSchemeOf s) DefeatSideScheme]
        choose "play searched support" (isTargetChoice card)
        pass
        supportsWith "01091" >>= (`shouldSatisfy` not . null)
    it "Mana Drain draws two then discards all cards printing the chosen resource" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Treacheries.mojo39046
        choose "drain mental" (isLabel "$label.mental")
        pass
        handOf (player1 tg) >>= (`shouldSatisfy` all (not . cardMatch (CardWithResource Mental)))

  describe "Horror and Sci-Fi" do
    it "Bandolier spends a stake to add one ATK and piercing to one basic attack" do
      tg <- testGame ["spider-man"]
      inGame tg do
        changeForm (player1 tg)
        pass
        clearHand (player1 tg)
        resource <- putInHand (player1 tg) "01088"
        void $ setEncounterDeck [Schemes.bombScare, Schemes.bombScare] []
        card <- revealNow (player1 tg) Attachments.mojo39048
        choose "take the stakes" (isLabel "$label.mojoTakeStakes")
        payWith [resource]
        pass
        let aid = AttachmentId $ unCardId $ ecCardId card
        getCountersOn aid `shouldReturn` 3
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) ToughStatus]
        choose "make a basic attack" (isHeroAttack $ player1 tg)
        choose "use a stake" (isAbilityOf aid)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        getCountersOn aid `shouldReturn` 2
    it "Cultist finds The Kraken and discards itself after activating" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Minions.mojo39050, Treacheries.advance] []
        mid <- addMinion (player1 tg) Minions.mojo39049
        resolve [MinionMessage mid MinionSchemes]
        minionDamageOf mid `shouldReturn` Nothing
        selectList (MinionIs Minions.mojo39050) >>= (`shouldSatisfy` not . null)
    it "Vampire heals all its damage after damaging a character" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mojo39051
        resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 2 FromAbility)]
        resolve [MinionMessage mid (MinionBeginAttack $ player1 tg)]
        pass
        minionDamageOf mid `shouldReturn` Just 0
    it "The Kraken damages every other character after activating" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mojo39050
        victim <- addMinion (player1 tg) Minions.hydraSoldier
        before <- damageOf (player1 tg)
        resolve [MinionMessage mid MinionSchemes]
        damageOf (player1 tg) `shouldReturn` before + 1
        minionDamageOf victim `shouldReturn` Just 1
        minionDamageOf mid `shouldReturn` Just 0
    it "Werewolf Pack sacrifices its captured ally instead of being defeated once" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- putIntoPlay (player1 tg) "01059"
        mid <- addMinion (player1 tg) Minions.mojo39052
        alliesWith "01059" `shouldReturn` []
        resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 5 FromAbility)]
        minionDamageOf mid `shouldReturn` Just 0
        resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 5 FromAbility)]
        minionDamageOf mid `shouldReturn` Nothing
    it "Avalanche exhausts and damages a controlled character when engaging" do
      tg <- testGame ["spider-man"]
      inGame tg do
        before <- damageOf (player1 tg)
        _ <- addMinion (player1 tg) Minions.mojo39054
        isExhaustedNow (player1 tg) `shouldReturn` True
        damageOf (player1 tg) `shouldReturn` before + 1
    it "Magneto discards one more card each time he activates" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Minions.hydraSoldier, Minions.hydraSoldier, Minions.hydraSoldier] []
        mid <- addMinion (player1 tg) Minions.mojo39056
        before <- length <$> handOf (player1 tg)
        resolve [MinionMessage mid MinionSchemes]
        length <$> handOf (player1 tg) `shouldReturn` before - 1
        resolve [MinionMessage mid MinionSchemes]
        length <$> handOf (player1 tg) `shouldReturn` before - 3
    it "Pyro deals indirect damage before his attack" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mojo39057
        before <- damageOf (player1 tg)
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        damageOf (player1 tg) `shouldReturn` before + 4
    it "Toad returns its stolen upgrade to its owner's hand on defeat" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- putIntoPlay (player1 tg) "01093"
        mid <- addMinion (player1 tg) Minions.mojo39058
        upgradeCodes >>= (`shouldSatisfy` notElem "01093")
        resolve [MinionMessage mid MinionDefeated]
        handOf (player1 tg) >>= (`shouldSatisfy` elem card)
    it "Blob cannot take more than two damage from one attack" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mojo39055
        resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 5 $ FromPlayerAttack $ player1 tg)]
        minionDamageOf mid `shouldReturn` Just 2
    it "ICE-Teroid M gives all minions guard and patrol" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hydraSoldier
        _ <- revealNow (player1 tg) Schemes.mojo39059
        pass
        getModifiers mid >>= (`shouldSatisfy` elem (KeywordModifier Guard))
        getModifiers mid >>= (`shouldSatisfy` elem (KeywordModifier Patrol))

  describe "Sitcom" do
    it "Family Matters blanks only its owner's supports" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        own <- putIntoPlay (player1 tg) "01091"
        other <- putIntoPlay (player2 tg) "01091"
        _ <- revealNow (player1 tg) Obligations.mojo39061
        pass
        getModifiers (supportOf own) >>= (`shouldSatisfy` elem Blank)
        getModifiers (supportOf other) >>= (`shouldSatisfy` notElem Blank)
    it "Growing Pains increases the owner's upgrade costs by two" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Obligations.mojo39062
        pass
        getModifiers (player1 tg) >>= (`shouldSatisfy` elem (AdditionalResourceCost (CardWithType UpgradeType) 2))
    it "The Odd Couple reduces its owner's ally limit by two" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Obligations.mojo39063
        pass
        getModifiers (player1 tg) >>= (`shouldSatisfy` elem (AllyLimitReduction 2))
    it "The One with the Breakup gives encounter cards peril" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Obligations.mojo39064
        pass
        mid <- addMinion (player1 tg) Minions.hydraSoldier
        getModifiers mid >>= (`shouldSatisfy` elem (KeywordModifier Peril))
    it "Watch Me Play records the confusion from looking up a rule" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Obligations.mojo39065
        pass
        choose "look up a rule" (isNamedAbility "$label.mojoLookUpRule")
        isConfused (player1 tg) `shouldReturn` True

  describe "Western" do
    it "Dead or Alive attaches to the minion with highest printed hit points" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.hydraSoldier
        mid <- addMinion (player1 tg) Minions.mojo39068
        _ <- revealNow (player1 tg) Attachments.mojo39067
        pass
        getModifiers mid >>= (`shouldSatisfy` elem (HitPointModifier 3))
    it "Card Shark counts different printed resources on its three discarded cards" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mojo39068
        _ <- stackDeck (player1 tg) ["01088", "01089", "01090", "01091"]
        before <- damageOf (player1 tg)
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        damageOf (player1 tg) >>= (`shouldSatisfy` (> before + 3))
    it "Gunslinger has quickstrike and offers its duel before attacking" do
      tg <- testGame ["spider-man"]
      inGame tg do
        clearHand (player1 tg)
        mid <- addMinion (player1 tg) Minions.mojo39069
        a <- getsGame (toAttrs . fromMaybe (error "Gunslinger missing") . HashMap.lookup mid . gameMinions)
        Minion.getModifiedKeywords a >>= (`shouldSatisfy` elem Quickstrike)
    it "A Game of Cards draws five then lets the player choose five to discard" do
      tg <- testGame ["spider-man"]
      inGame tg do
        clearHand (player1 tg)
        _ <- revealNow (player1 tg) Treacheries.mojo39070
        pass
        cards <- handOf (player1 tg)
        for_ (take 5 cards) $ \c -> choose "discard drawn card" (isTargetChoice c)
        pass
        handOf (player1 tg) `shouldReturn` []
