module Marvel.Hero.JessicaJonesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.JessicaJones.Helpers qualified as JL
import Marvel.Minion.Cards qualified as Minions
import Marvel.Status
import Marvel.Modifier
import Marvel.Matchers qualified as Matchers
import Marvel.Query (selectJust, selectList)
import Marvel.Game.Source qualified as Engine
import TestImport

asJessica :: IdentityId -> GameAppT ()
asJessica p = changeForm p >> pass >> clearHand p

pay :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
pay p card codes = traverse (putInHand p) codes >>= playWith card

spec :: Spec
spec = describe "Jessica Jones" do
  it "puts permanent Alias Investigations into play during setup" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      aliases <- supportsWith "61002"
      length aliases `shouldBe` 1
      JL.evidenceCount (player1 tg) `shouldReturn` 0

  it "starts with eleven hit points and six alter-ego hand size" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      hitPointsOf (player1 tg) `shouldReturn` 11
      handSizeOf (player1 tg) `shouldReturn` 6

  it "has hero THW three, ATK two and DEF one" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      currentIdentity <$> getPlayer p >>= \case
        HeroSide h -> do
          Hero.getModifiedThwart (toAttrs h) `shouldReturn` 3
          Hero.getModifiedAttack (toAttrs h) `shouldReturn` 2
          Hero.getModifiedDefense (toAttrs h) `shouldReturn` 1
        _ -> error "Jessica should be a hero"

  it "Incognito Mode changes to hero without the ordinary form-change action" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      choose "Incognito Mode" (isNamedAbility "$label.incognitoMode")
      (.playerIdentitySide) <$> playerAttrs p `shouldReturn` A
      modifiers <- Engine.getModifiers p
      modifiers `shouldSatisfy` elem CannotChangeToAlterEgoForm

  it "Gather Evidence responds after a basic attack" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      choose "Gather Evidence" (isNamedAbility "$label.gatherEvidence")
      JL.evidenceCount p `shouldReturn` 1
      villainDamageNow `shouldReturn` 2

  it "Gather Evidence responds after a basic thwart" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      placeMainThreat 4
      choose "basic thwart" (isHeroThwart p)
      chooseMainScheme
      choose "Gather Evidence" (isNamedAbility "$label.gatherEvidence")
      JL.evidenceCount p `shouldReturn` 1

  it "Big Mistake deals three damage without evidence" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      card <- putInHand p "61004"
      pay p card ["01088"]
      chooseVillain
      choose "base damage" (isLabel "$label.bigMistakeBase")
      villainDamageNow `shouldReturn` 3
      JL.evidenceCount p `shouldReturn` 0

  it "Big Mistake can spend evidence for five damage and piercing" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      alias <- testHead <$> supportsWith "61002"
      resolve [PlaceCounters (SupportRef $ supportId alias) 1]
      vid <- villainIdNow
      resolve [GiveStatus (VillainRef vid) ToughStatus]
      card <- putInHand p "61004"
      pay p card ["01088"]
      chooseVillain
      choose "piercing" (isLabel "$label.bigMistakePiercing")
      villainDamageNow `shouldReturn` 5
      JL.evidenceCount p `shouldReturn` 0

  it "Snooping Around removes four threat in alter ego and adds two evidence" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      _ <- stackEncounterDeck [Minions.hydraBomber]
      card <- putInHand p "61006"
      playWith card []
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` (before - 4)
      JL.evidenceCount p `shouldReturn` 2

  it "Snooping Around is unavailable in hero form" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      placeMainThreat 4
      card <- putInHand p "61006"
      offers (isPlay card) `shouldReturn` False

  it "Luke Cage enters play tough with two THW and two ATK" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "61003"
      a <- allyIn "61003"
      allyTough a `shouldBe` True
      Ally.getModifiedThwart a `shouldReturn` 2
      Ally.getModifiedAttack a `shouldReturn` 2

  it "Luke Cage's action exhausts and replaces a tough card spent on its damage cost" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      card <- putIntoPlay p "61003"
      choose "Luke Cage" (\case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf card) && abilityIndex ab == 1; _ -> False)
      a <- allyIn "61003"
      allyExhausted a `shouldBe` True
      allyTough a `shouldBe` True
      allyDamage a `shouldBe` 0

  it "Leather Jacket adds two DEF" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      _ <- putIntoPlay p "61011"
      currentIdentity <$> getPlayer p >>= \case
        HeroSide h -> Hero.getModifiedDefense (toAttrs h) `shouldReturn` 3
        _ -> error "expected hero"

  it "4K Digital Camcorder discards a two-health non-Elite minion without evidence" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      camera <- putIntoPlay p "61009"
      mid <- addMinion p Minions.hydraBomber
      choose "Camcorder" (isAbilityOfCard camera)
      choose "minion" (isTargetChoice mid)
      minionDamageOf mid `shouldReturn` Nothing
      JL.evidenceCount p `shouldReturn` 0

  it "4K Digital Camcorder spends remaining hit points minus two as evidence" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      camera <- putIntoPlay p "61009"
      alias <- testHead <$> supportsWith "61002"
      resolve [PlaceCounters (SupportRef $ supportId alias) 4]
      mid <- addMinion p Minions.armoredGuard
      choose "Camcorder" (isAbilityOfCard camera)
      choose "minion" (isTargetChoice mid)
      minionDamageOf mid `shouldReturn` Nothing
      JL.evidenceCount p `shouldReturn` 3

  it "Innate Aggression adds one ATK" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      _ <- putIntoPlay p "61034"
      currentIdentity <$> getPlayer p >>= \case
        HeroSide h -> Hero.getModifiedAttack (toAttrs h) `shouldReturn` 3
        _ -> error "expected hero"

  it "Innate Perception adds one THW" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      _ <- putIntoPlay p "61036"
      currentIdentity <$> getPlayer p >>= \case
        HeroSide h -> Hero.getModifiedThwart (toAttrs h) `shouldReturn` 4
        _ -> error "expected hero"

  it "Innate Inspiration gives each controlled ally one additional hit point" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "61037"
      _ <- putIntoPlay p "61003"
      a <- allyIn "61003"
      Ally.getModifiedHitPoints a `shouldReturn` 4

  it "Breakthrough readies Jessica after evidence is placed and shuffles into her deck" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      breakthrough <- putInHand p "61005"
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      choose "Gather Evidence" (isNamedAbility "$label.gatherEvidence")
      playWith breakthrough []
      isExhaustedPlayer p `shouldReturn` False
      deck <- unDeck . playerIdentityDeck <$> playerAttrs p
      map toCardCode deck `shouldSatisfy` elem "61005"

  it "Reluctant Flier prevents a three-damage attack then deals one ability damage" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      flier <- putIntoPlay p "61013"
      start (damageIdentity p (toSource p) $ toDamage 3 FromAttack)
      choose "Reluctant Flier" (isAbilityOfCard flier)
      damageOf p `shouldReturn` 1
      codes <- upgradeCodes
      codes `shouldSatisfy` notElem "61013"

  it "Reluctant Flier is unavailable for a two-damage attack" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      flier <- putIntoPlay p "61013"
      start (damageIdentity p (toSource p) $ toDamage 2 FromAttack)
      offers (isAbilityOfCard flier) `shouldReturn` False

  it "Grapnel Launcher makes a four-THW basic thwart even while exhausted" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      launcher <- putIntoPlay p "61023"
      resolve [IdentityMessage p ExhaustedIdentity]
      placeMainThreat 5
      before <- mainSchemeThreatNow
      choose "Grapnel Launcher" (isAbilityOfCard launcher)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` (before - 4)
      isExhaustedPlayer p `shouldReturn` True
      codes <- upgradeCodes
      codes `shouldSatisfy` notElem "61023"

  it "Piecing It All Together does not count against the player side-scheme limit" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      card <- putIntoPlay p "61007"
      let sid = SideSchemeId $ unCardId $ pcCardId card
      modifiers <- Engine.getModifiers (SideSchemeRef sid)
      modifiers `shouldSatisfy` elem NotCountedAgainstPlayerSideSchemeLimit
      _ <- putIntoPlay p "61028"
      schemes <- getsGame (HashMap.keys . gameSideSchemes)
      schemes `shouldSatisfy` elem sid

  it "Piecing It All Together draws three cards for the defeating player" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      card <- putIntoPlay p "61007"
      let sid = SideSchemeId $ unCardId $ pcCardId card
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved (toSource p) 3]
      choose "basic thwart" (isHeroThwart p)
      choose "Piecing It All Together" (isTargetChoice $ SchemeSideSchemeId sid)
      pass
      length <$> handOf p `shouldReturn` 3

  it "Second Chance shuffles identity-specific cards out of the discard pile" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      _ <- putInDiscard p "61004"
      _ <- putInDiscard p "01088"
      scheme <- putIntoPlay p "61028"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      choose "basic thwart" (isHeroThwart p)
      choose "Second Chance" (isTargetChoice $ SchemeSideSchemeId sid)
      choose "shuffle identity cards" (isLabel "$label.secondChanceShuffle")
      pass
      deck <- unDeck . playerIdentityDeck <$> playerAttrs p
      map toCardCode deck `shouldSatisfy` elem "61004"
      discard <- map toCardCode <$> discardOf p
      discard `shouldSatisfy` elem "01088"
      discard `shouldSatisfy` notElem "61004"

  it "Run Them to Ground skips the next villain phase and prevents villain damage" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      scheme <- putIntoPlay p "61020"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved (toSource p) 1]
      choose "basic thwart" (isHeroThwart p)
      choose "Run Them to Ground" (isTargetChoice $ SchemeSideSchemeId sid)
      pass
      modifiers <- Engine.getModifiers GameRef
      modifiers `shouldSatisfy` elem SkipNextVillainPhase
      vid <- villainIdNow
      villainModifiers <- Engine.getModifiers (VillainRef vid)
      villainModifiers `shouldSatisfy` elem CannotTakeDamage
      cards <- dealtTo p
      length cards `shouldBe` 1

  it "Lay Low optionally changes to alter ego and heals the alter ego's REC" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      setDamage p 5
      scheme <- putIntoPlay p "61039"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      choose "basic thwart" (isHeroThwart p)
      choose "Lay Low" (isTargetChoice $ SchemeSideSchemeId sid)
      choose "change to alter ego" (isLabel "$label.changeToB")
      pass
      damageOf p `shouldReturn` 1
      (.playerIdentitySide) <$> playerAttrs p `shouldReturn` B

  it "Alias Investigations adds two evidence after a side scheme is defeated" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      alias <- testHead <$> supportsWith "61002"
      scheme <- putIntoPlay p "61028"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      choose "basic thwart" (isHeroThwart p)
      choose "Second Chance" (isTargetChoice $ SchemeSideSchemeId sid)
      choose "skip shuffle" (isLabel "$label.done")
      choose "Alias Investigations" (\case UseAbility ab -> abilitySource ab == SupportRef (supportId alias); _ -> False)
      choose "keep investigating" (isLabel "$label.done")
      pass
      JL.evidenceCount p `shouldReturn` 2

  it "Calling in Favors spends two evidence and reduces the searched ally's cost by two" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      alias <- testHead <$> supportsWith "61002"
      favors <- putIntoPlay p "61008"
      resolve [PlaceCounters (SupportRef $ supportId alias) 2]
      allies <- stackDeck p ["61003", "01088"]
      payment <- putInHand p "01090"
      choose "Calling in Favors" (isAbilityOfCard favors)
      choose "Luke Cage" (isTargetChoice $ testHead allies)
      payWith [payment]
      a <- allyIn "61003"
      allyTough a `shouldBe` True
      JL.evidenceCount p `shouldReturn` 0

  it "Circumstantial Evidence adds evidence after an enemy is defeated" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      evidence <- putIntoPlay p "61010"
      mid <- addMinion p Minions.hydraBomber
      choose "basic attack" (isHeroAttack p)
      choose "Hydra Bomber" (isTargetChoice $ EnemyMinionId mid)
      choose "Circumstantial Evidence" (isAbilityOfCard evidence)
      pass
      JL.evidenceCount p `shouldReturn` 1
      upgradeIsExhausted (upgradeOf evidence) `shouldReturn` True

  it "Now I'm Ticked Off changes Jessica to hero after a villain activation and deals five" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      ticked <- putIntoPlay p "61012"
      _ <- setEncounterDeck [Cards.jl62029, Cards.jl62029] []
      beforeThreat <- mainSchemeThreatNow
      mid <- mainSchemeId
      resolve [MainSchemeMessage mid $ MainSchemeThreatRemoved (toSource p) beforeThreat]
      vid <- villainIdNow
      start [VillainMessage vid $ VillainSchemesAgainst p]
      choose "Now I'm Ticked Off forced response" (isAbilityOfCard ticked)
      (.playerIdentitySide) <$> playerAttrs p `shouldReturn` A
      villainDamageNow `shouldReturn` 5
      codes <- upgradeCodes
      codes `shouldSatisfy` notElem "61012"

  it "Entrapment changes to hero and deals damage equal to threat actually placed" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      trap <- putIntoPlay p "61024"
      _ <- setEncounterDeck [Cards.jl62029, Cards.jl62029] []
      beforeThreat <- mainSchemeThreatNow
      mid <- mainSchemeId
      resolve [MainSchemeMessage mid $ MainSchemeThreatRemoved (toSource p) beforeThreat]
      vid <- villainIdNow
      before <- mainSchemeThreatNow
      start [VillainMessage vid $ VillainSchemesAgainst p]
      choose "Entrapment" (isAbilityOfCard trap)
      after <- mainSchemeThreatNow
      villainDamageNow `shouldReturn` (after - before)
      (.playerIdentitySide) <$> playerAttrs p `shouldReturn` A
      codes <- upgradeCodes
      codes `shouldSatisfy` notElem "61024"

  it "Stakeout adds two threat removal to a thwart and evidence when it defeats its scheme" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      stakeout <- putIntoPlay p "61014"
      encounter <- revealNow p SideSchemes.breakinAndTakin
      pass
      let sid = sideSchemeOf encounter
      choose "Stakeout" (isAbilityOfCard stakeout)
      choose "attach to the encounter side scheme" (isTargetChoice sid)
      resolve [SideSchemeMessage sid $ SideSchemePlaceThreat 2]
      choose "basic thwart" (isHeroThwart p)
      choose "attached scheme" (isTargetChoice $ SchemeSideSchemeId sid)
      choose "Stakeout interrupt" (isAbilityOfCard stakeout)
      pass
      JL.evidenceCount p `shouldReturn` 2

  it "Defend Our City searches for a Team-Up and reduces Team-Up costs for the phase" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      found <- stackDeck p ["61027", "01088"]
      scheme <- putIntoPlay p "61029"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved (toSource p) 1]
      choose "basic thwart" (isHeroThwart p)
      choose "Defend Our City" (isTargetChoice $ SchemeSideSchemeId sid)
      choose "search" (isLabel "$label.searchDeck")
      choose "Unbreakable Bond" (isTargetChoice $ testHead found)
      pass
      cards <- map toCardCode <$> handOf p
      cards `shouldSatisfy` elem "61027"
      modifiers <- Engine.getModifiers p
      modifiers `shouldSatisfy` elem (ResourceCostReductionFor CardWithTeamUp 1)

  it "Gather Evidence after basic DEF waits for attack damage and triggers only once" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      _ <- setEncounterDeck [Cards.jl62029, Cards.jl62029] []
      vid <- villainIdNow
      start [VillainMessage vid $ VillainAttacks p]
      choose "Jessica defends" isDefend
      damageOf p `shouldReturn` 1
      choose "Gather Evidence" (isNamedAbility "$label.gatherEvidence")
      JL.evidenceCount p `shouldReturn` 1
      offers (isNamedAbility "$label.gatherEvidence") `shouldReturn` False

  it "two Stakeouts both award evidence when their combined bonus defeats a scheme" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      first <- putIntoPlay p "61014"
      second <- putIntoPlay p "61014"
      encounter <- revealNow p SideSchemes.breakinAndTakin
      pass
      let sid = sideSchemeOf encounter
      choose "first Stakeout" (isAbilityOfCard first)
      choose "attach first Stakeout" (isTargetChoice sid)
      choose "second Stakeout" (isAbilityOfCard second)
      choose "attach second Stakeout" (isTargetChoice sid)
      resolve [SideSchemeMessage sid $ SideSchemePlaceThreat 4]
      choose "basic thwart" (isHeroThwart p)
      choose "attached scheme" (isTargetChoice $ SchemeSideSchemeId sid)
      choose "first Stakeout interrupt" (isAbilityOfCard first)
      choose "second Stakeout interrupt" (isAbilityOfCard second)
      pass
      JL.evidenceCount p `shouldReturn` 4

  it "Stakeout awards no evidence when its boosted thwart leaves threat" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      asJessica p
      stakeout <- putIntoPlay p "61014"
      encounter <- revealNow p SideSchemes.breakinAndTakin
      pass
      let sid = sideSchemeOf encounter
      choose "Stakeout" (isAbilityOfCard stakeout)
      choose "attach Stakeout" (isTargetChoice sid)
      resolve [SideSchemeMessage sid $ SideSchemePlaceThreat 4]
      choose "basic thwart" (isHeroThwart p)
      choose "attached scheme" (isTargetChoice $ SchemeSideSchemeId sid)
      choose "Stakeout interrupt" (isAbilityOfCard stakeout)
      pass
      JL.evidenceCount p `shouldReturn` 0

  it "Piecing It All Together has six fixed threat in a multiplayer game" do
    tg <- testGame ["jessica-jones", "luke-cage"]
    inGame tg do
      card <- putIntoPlay (player1 tg) "61007"
      sideSchemeThreat (SideSchemeId $ unCardId $ pcCardId card) `shouldReturn` 6

  it "aborting Calling in Favors preserves the searched ally in its deck" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      alias <- testHead <$> supportsWith "61002"
      favors <- putIntoPlay p "61008"
      resolve [PlaceCounters (SupportRef $ supportId alias) 2]
      found <- stackDeck p ["61003", "01088"]
      payment <- putInHand p "01090"
      choose "Calling in Favors" (isAbilityOfCard favors)
      choose "searched Luke Cage" (isTargetChoice $ testHead found)
      acid <- fromMaybe (error "missing searched ally payment") <$> getActiveCost
      start [AbortPayment acid]
      pass
      deck <- unDeck . playerIdentityDeck <$> playerAttrs p
      deck `shouldSatisfy` elem (testHead found)
      handOf p >>= (`shouldSatisfy` elem payment)
      handOf p >>= (`shouldSatisfy` notElem (testHead found))
      alliesWith "61003" `shouldReturn` []

  it "Now I'm Ticked Off damages the activating villain when it is not the active villain" do
    tg <- testGameAgainst "07001" ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      ticked <- putIntoPlay p "61012"
      active <- selectJust Matchers.ActiveVillain
      activating <- testHead . filter (/= active) <$> selectList Matchers.AnyVillain
      start [VillainMessage activating $ VillainSchemesAgainst p]
      choose "Now I'm Ticked Off" (isAbilityOfCard ticked)
      damage <- getsGame (HashMap.map villainDamage . gameVillains)
      lookup activating damage `shouldBe` Just 5
      lookup active damage `shouldBe` Just 0
