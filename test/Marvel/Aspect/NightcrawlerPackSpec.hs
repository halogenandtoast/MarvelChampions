module Marvel.Aspect.NightcrawlerPackSpec (spec) where

import Control.Monad (replicateM_)

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Runner qualified as Ally
import Marvel.Boost (boostCount)
import Marvel.Keyword (Keyword (Villainous))
import Marvel.Effect.Types qualified as Effect
import Marvel.Effect.Lasting (Lasting (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers (EntityMatcher (EnemyEntity), EnemyMatcher (EnemyWithId))
import Marvel.Modifier
import Marvel.Phase
import Marvel.SideScheme.Types ()
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

paying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
paying p code codes = do
  card <- putInHand p code
  payment <- traverse (putInHand p) codes
  playWith card payment
  pure card

schemeIdOf :: PlayerCard -> SideSchemeId
schemeIdOf = SideSchemeId . unCardId . pcCardId

spec :: Spec
spec = describe "Nightcrawler pack" do
  it "Rogue damages a different friendly character and gains its current traits and base powers" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      rogue <- putIntoPlay p "48012"
      choose "Rogue copies Nightcrawler" (\c -> isAbilityOfCard rogue c && case c of UseAbility a -> abilityIndex a == 1; _ -> False)
      choose "damage Nightcrawler" (isTargetChoice p)
      pass
      damageOf p `shouldReturn` 1
      attrs <- allyIn "48012"
      Ally.getModifiedAttack attrs `shouldReturn` 3
      Ally.getModifiedThwart attrs `shouldReturn` 4
      getModifiers (allyId attrs) >>= (`shouldSatisfy` elem (TraitModifier Trait.XMen))
      offers (\c -> isAbilityOfCard rogue c && case c of UseAbility a -> abilityIndex a == 1; _ -> False) `shouldReturn` False

  it "Rogue copies base powers rather than bonuses and loses the copy at round end" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "48004"
      rogue <- putIntoPlay p "48012"
      choose "Rogue's copy" (\c -> isAbilityOfCard rogue c && case c of UseAbility a -> abilityIndex a == 1; _ -> False)
      choose "copy Nightcrawler" (isTargetChoice p)
      pass
      attrs <- allyIn "48012"
      Ally.getModifiedAttack attrs `shouldReturn` 3
      runNow [EndRound]
      pass
      current <- allyIn "48012"
      Ally.getModifiedAttack current `shouldReturn` 2
      Ally.getModifiedThwart current `shouldReturn` 2

  it "Northstar does not offer his interrupt for a card with no boost icons" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      northstar <- putIntoPlay p "48013"
      _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
      v <- villainIdNow
      runNow [VillainMessage v (VillainAttacks p)]
      choose "decline defense" isNoDefender
      offers (\choice -> isAbilityOfCard northstar choice && case choice of UseAbility ab -> abilityIndex ab == 1; _ -> False) `shouldReturn` False
      allyDamage <$> allyIn "48013" `shouldReturn` 0

  it "Northstar can cancel an amplify icon added to a card with no printed icons" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      northstar <- putIntoPlay p "48013"
      v <- villainIdNow
      eid <- getRandom
      withGame_ $ entitiesL . effectsL . at eid ?~ Effect.Effect (Lasting $ Effect.EffectAttrs
        eid "engine-lasting" GameRef (EnemyEntity $ EnemyWithId $ EnemyVillainId v) [AmplifyIconModifier 1] Nothing)
      _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
      runNow [VillainMessage v (VillainAttacks p)]
      choose "decline defense" isNoDefender
      choose "Northstar cancels amplify" (isAbilityOfCard northstar)
      pass
      allyDamage <$> allyIn "48013" `shouldReturn` 1
      damageOf p `shouldReturn` 2

  for_ [False, True] \villainousMinion ->
    it ("declining Northstar's interrupt closes the flip window once: " <> show villainousMinion) do
      tg <- testGame ["nightcrawler"]
      inGame tg do
        let p = player1 tg
            response card choice = isAbilityOfCard card choice && case choice of UseAbility ab -> abilityIndex ab == 1; _ -> False
        asHero p
        northstar <- putIntoPlay p "48013"
        enemy <- if villainousMinion then do
          mid <- addMinion p (Minions.hydraRegular {cdKeywords = fromList [Villainous]})
          pure $ EnemyMinionId mid
        else EnemyVillainId <$> villainIdNow
        _ <- stackEncounterDeck [Minions.vulture]
        runNow [case enemy of EnemyVillainId v -> VillainMessage v (VillainAttacks p); EnemyMinionId m -> MinionMessage m (MinionAttacks p); _ -> error "unexpected enemy"]
        choose "decline defense" isNoDefender
        offers (response northstar) `shouldReturn` True
        choose "decline Northstar once" (isLabel "$label.noResponse")
        offers (response northstar) `shouldReturn` False

  it "Northstar takes a damage cost and cancels only the boost card's icons" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      northstar <- putIntoPlay p "48013"
      _ <- stackEncounterDeck [Minions.vulture]
      v <- villainIdNow
      runNow [VillainMessage v (VillainAttacks p)]
      choose "decline defense" isNoDefender
      choose "Northstar cancels boost icons" (isAbilityOfCard northstar)
      pass
      allyDamage <$> allyIn "48013" `shouldReturn` 1
      damageOf p `shouldReturn` 2

  it "Change of Fortune exhausts to draw two after its controller defeats an enemy in villain phase" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      fortune <- putIntoPlay p "48014"
      m <- addMinion p Minions.hydraRegular
      withGame_ $ phaseL .~ VillainPhase
      runNow [MinionMessage m (MinionDamaged (IdentityRef p) $ toDamage 10 FromAbility)]
      choose "Change of Fortune" (isAbilityOfCard fortune)
      pass
      length <$> handOf p `shouldReturn` 2
      upgradeIsExhausted (upgradeIdOf fortune) `shouldReturn` True

  it "Change of Fortune is unavailable after a player-phase defeat" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      fortune <- putIntoPlay p "48014"
      m <- addMinion p Minions.hydraRegular
      runNow [MinionMessage m (MinionDamaged (IdentityRef p) $ toDamage 10 FromAbility)]
      offers (isAbilityOfCard fortune) `shouldReturn` False
      pass
      length <$> handOf p `shouldReturn` 0

  it "Under Control deals four to its host after a hero defends without damage" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.tombstone
      under <- putInHand p "48015"
      playOn under m []
      pass
      runNow [MinionMessage m (MinionAttacks p)]
      choose "defend" isDefend
      choose "Under Control" (isAbilityOfCard under)
      pass
      minionDamageOf m `shouldReturn` Just 4
      damageOf p `shouldReturn` 0

  it "resolves Tiger Shark's forced tough before the optional Under Control response" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.tigerShark
      under <- putInHand p "48015"
      playOn under m []
      pass
      runNow [MinionMessage m (MinionAttacks p)]
      choose "defend" isDefend
      offers (isAbilityOfCard under) `shouldReturn` False
      choose "Tiger Shark gains tough" (isAbilityOf m)
      choose "Under Control" (isAbilityOfCard under)
      pass
      minionDamageOf m `shouldReturn` Just 0

  it "Under Control does not respond to an undefended attack" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.tigerShark
      under <- putInHand p "48015"
      playOn under m []
      pass
      runNow [MinionMessage m (MinionAttacks p)]
      choose "decline defense" isNoDefender
      offers (isAbilityOfCard under) `shouldReturn` False
      pass
      minionDamageOf m `shouldReturn` Just 0

  it "Come Get Me Bub discards through a minion, puts it into play, heals three and grants tough" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      resolve [IdentityMessage p (IdentityDamaged GameRef $ toDamage 5 FromAbility)]
      _ <- setEncounterDeck [Treacheries.assault, Minions.hydraRegular] []
      _ <- paying p "48016" []
      pass
      damageOf p `shouldReturn` 2
      playerIdentityTough <$> playerAttrs p `shouldReturn` True
      length . playerIdentityMinions <$> playerAttrs p `shouldReturn` 1

  it "Riposte boosts DEF for one attack and deals three when that attack deals no damage" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.tigerShark
      riposte <- putInHand p "48018"
      resource <- putInHand p "01086"
      runNow [MinionMessage m (MinionAttacksWithChange p 2 [])]
      choose "defend" isDefend
      playWith riposte [resource]
      pass
      choose "Riposte damage" (isAbilityOf $ EventRef $ EventId $ unCardId $ pcCardId riposte)
      choose "Tiger Shark gains tough" (isAbilityOf m)
      pass
      damageOf p `shouldReturn` 0
      minionDamageOf m `shouldReturn` Just 3
      getModifiers p >>= (`shouldSatisfy` notElem (DefenseModifier 2))

  it "Astonishing X-Men removes threat after X-Men defend without damage, then stuns and confuses all enemies" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      scheme <- putIntoPlay p "48020"
      let sid = schemeIdOf scheme
      sideSchemeThreat sid `shouldReturn` 5
      m <- addMinion p Minions.tombstone
      runNow [MinionMessage m (MinionAttacks p)]
      choose "Nightcrawler defends" isDefend
      choose "Astonishing X-Men response" (isAbilityOf sid)
      pass
      sideSchemeThreat sid `shouldReturn` 4
      resolve [SideSchemeMessage sid (SideSchemeThreatRemoved (IdentityRef p) 4)]
      enemy <- getsGame (toAttrs . fromMaybe (error "missing minion") . HashMap.lookup m . gameMinions)
      minionStunned enemy `shouldBe` True
      minionConfused enemy `shouldBe` True

  it "Gambit tucks one of the top three encounter cards and uses its boost icons for both powers" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      cards <- stackEncounterDeck [Minions.vulture, Treacheries.assault, Treacheries.advance]
      gambit <- paying p "48021" ["01088", "01086"]
      choose "Gambit's response" (isAbilityOfCard gambit)
      choose "tuck Vulture" (isTargetChoice $ testHead cards)
      pass
      attrs <- allyIn "48021"
      allyHeldTreacheries attrs `shouldBe` [testHead cards]
      Ally.getModifiedAttack attrs `shouldReturn` boostCount (cdBoostIcons $ getCardDef $ testHead cards)
      Ally.getModifiedThwart attrs `shouldReturn` boostCount (cdBoostIcons $ getCardDef $ testHead cards)
      deck <- scenarioEncounterDeckNow
      take 2 deck `shouldBe` take 2 (testTail cards)

  it "Combine Forces exhausts distinct X-Force and X-Men characters to defeat a non-Elite minion" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      caliban <- putIntoPlay p "40014"
      m <- addMinion p Minions.hydraRegular
      _ <- paying p "48031" ["01086"]
      whenM (offers $ isTargetChoice $ allyIdOf caliban) $ choose "exhaust Caliban" (isTargetChoice $ allyIdOf caliban)
      whenM (offers $ isTargetChoice p) $ choose "exhaust Nightcrawler" (isTargetChoice p)
      choose "defeat non-Elite" (isTargetChoice m)
      pass
      minionDamageOf m `shouldReturn` Nothing
      isExhaustedPlayer p `shouldReturn` True
      allyExhausted <$> allyIn "40014" `shouldReturn` True

  it "Combine Forces cannot be played when no X-Force character can pay its cost" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- addMinion p Minions.hydraRegular
      combine <- putInHand p "48031"
      _ <- putInHand p "01088"
      offers (isPlay combine) `shouldReturn` False

  it "Gunboat Diplomacy divides the combined current THW into both threat removal and damage" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      caliban <- putIntoPlay p "40014"
      _ <- revealNow p Schemes.breakinAndTakin
      pass
      _ <- addMinion p Minions.tigerShark
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main (MainSchemePlaceThreat 4)]
      before <- mainSchemeThreatNow
      _ <- paying p "48032" ["01086"]
      whenM (offers $ isTargetChoice $ allyIdOf caliban) $ choose "exhaust X-Force" (isTargetChoice $ allyIdOf caliban)
      whenM (offers $ isTargetChoice p) $ choose "exhaust X-Men" (isTargetChoice p)
      replicateM_ 3 chooseMainScheme
      mainSchemeThreatNow `shouldReturn` before
      replicateM_ 3 chooseVillain
      pass
      mainSchemeThreatNow `shouldReturn` before - 3
      villainDamageNow `shouldReturn` 3

  it "Astonishing X-Men also rewards a tough X-Men ally defending without taking damage" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      scheme <- putIntoPlay p "48020"
      rogue <- putIntoPlay p "48012"
      resolve [GiveStatus (AllyRef $ allyIdOf rogue) ToughStatus]
      m <- addMinion p Minions.tigerShark
      runNow [MinionMessage m (MinionAttacks p)]
      choose "Rogue defends" (\case AllyDefend aid _ -> aid == allyIdOf rogue; _ -> False)
      choose "Astonishing X-Men's ally response" (isAbilityOf $ schemeIdOf scheme)
      pass
      sideSchemeThreat (schemeIdOf scheme) `shouldReturn` 4
      allyDamage <$> allyIn "48012" `shouldReturn` 0

  it "Rogue gains no copied bonus when paying her damage cost defeats the other ally" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      rogue <- putIntoPlay p "48012"
      other <- putIntoPlay p "01083"
      resolve [AllyMessage (allyIdOf other) (AllyDamaged GameRef $ toDamage 2 FromAbility)]
      choose "Rogue copies ally" (\c -> isAbilityOfCard rogue c && case c of UseAbility a -> abilityIndex a == 1; _ -> False)
      choose "damage Mockingbird" (isTargetChoice $ allyIdOf other)
      pass
      alliesWith "01083" `shouldReturn` []
      attrs <- allyIn "48012"
      Ally.getModifiedThwart attrs `shouldReturn` 2
      Ally.getModifiedAttack attrs `shouldReturn` 2

  it "Riposte does not deal its delayed damage when the attack damages Nightcrawler" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.tigerShark
      riposte <- putInHand p "48018"
      resource <- putInHand p "01086"
      runNow [MinionMessage m (MinionAttacksWithChange p 5 [])]
      choose "defend" isDefend
      playWith riposte [resource]
      pass
      minionDamageOf m `shouldReturn` Just 0
      damageOf p >>= (`shouldSatisfy` (> 0))

  it "Rogue continuously follows the chosen identity's form and traits until the round ends" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      rogue <- putIntoPlay p "48012"
      choose "Rogue copies Nightcrawler" (\c -> isAbilityOfCard rogue c && case c of UseAbility a -> abilityIndex a == 1; _ -> False)
      choose "damage Nightcrawler" (isTargetChoice p)
      pass
      bamf <- putInHand p "48006"
      event <- putInHand p "48010"
      playWith event []
      whenM (offers $ isTargetChoice bamf) $ choose "discard Bamf" (isTargetChoice bamf)
      pass
      attrs <- allyIn "48012"
      Ally.getModifiedAttack attrs `shouldReturn` 2
      Ally.getModifiedThwart attrs `shouldReturn` 2
      getModifiers (allyId attrs) >>= (`shouldSatisfy` elem (TraitModifier Trait.Mutant))

  for_ [(True,3),(False,1)] $ \(retaliateFirst,expected) ->
    it ("Riposte and retaliate can resolve in either order against tough: " <> show retaliateFirst) do
      tg <- testGame ["nightcrawler"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putIntoPlay p "48004"
        m <- addMinion p Minions.tigerShark
        resolve [GiveStatus (MinionRef m) ToughStatus]
        riposte <- putInHand p "48018"
        resource <- putInHand p "01086"
        runNow [MinionMessage m (MinionAttacks p)]
        choose "defend" isDefend
        playWith riposte [resource]
        if retaliateFirst
          then do
            choose "retaliate first" (isLabeledAbility "$label.resolveRetaliate")
            choose "Riposte second" (isAbilityOf $ EventRef $ EventId $ unCardId $ pcCardId riposte)
          else do
            choose "Riposte first" (isAbilityOf $ EventRef $ EventId $ unCardId $ pcCardId riposte)
            choose "retaliate second" (isLabeledAbility "$label.resolveRetaliate")
        choose "Tiger Shark gains tough" (isAbilityOf m)
        pass
        minionDamageOf m `shouldReturn` Just expected
