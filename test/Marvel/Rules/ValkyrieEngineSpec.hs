module Marvel.Rules.ValkyrieEngineSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (windowAbility)
import Marvel.Ability.Types (AbilityType (..), HasAbilities (..))
import Marvel.Ally.Types qualified as Ally
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Cost.Types (Cost (..))
import Marvel.Matchers.Types hiding (ExhaustedIdentity)
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (HasModifiersFor (..), Modifier (..))
import Marvel.Query (selectList)
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as Scheme
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import Marvel.Window qualified as W
import Data.List qualified as List
import TestImport

-- These small fixture upgrades supply ordinary player-triggered abilities without
-- depending on a new hero pack's registration or card implementations.
data ProbeAction = OrdinaryProbe | BroadenAttack AllyId [EnemyId] | ReplaceDefenseWithAttack IdentityId EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ProbeUpgrade = ProbeUpgrade (Attrs Upgrade) [Ability] [(Target, [Modifier])] (Maybe AttachmentId) [(Natural, ProbeAction)]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsUpgrade ProbeUpgrade where
  toUpgradeAttrs (ProbeUpgrade a _ _ _ _) = a

instance HasAbilities ProbeUpgrade where
  getAbilities (ProbeUpgrade _ abilities _ _ _) = abilities

instance HasModifiersFor ProbeUpgrade where
  getModifiersFor _ target (ProbeUpgrade _ _ modifiers protected _) = do
    protection <- case protected of
      Just aid | target == AttachmentRef aid -> do
        friendly <- attachmentHasFriendlyHost aid
        pure [CannotBeDiscarded | friendly]
      _ -> pure []
    pure $ concat [ms | (t, ms) <- modifiers, t == target] <> protection

instance RunMessage ProbeUpgrade where
  runMessage msg (ProbeUpgrade a abilities modifiers protected actions) = L.runQueueT $ case msg of
    RanAbility _ source idx _ _ | source == UpgradeRef (upgradeId a) -> do
      for_ (List.lookup idx actions) $ \case
        BroadenAttack aid enemies -> L.resolveAllyAttackAgainstAll aid enemies
        ReplaceDefenseWithAttack ident enemy -> L.useAttackForDefense ident enemy
        OrdinaryProbe -> pure ()
      pure $ ProbeUpgrade a abilities modifiers protected actions
    _ -> (\a' -> ProbeUpgrade a' abilities modifiers protected actions) <$> runMessage msg a

installProbe :: IdentityId -> [(Natural, W.WindowMatcher, AbilityType, [Message])] -> [(Target, [Modifier])] -> Maybe AttachmentId -> [(Natural, ProbeAction)] -> GameAppT PlayerCard
installProbe p windows modifiers protected actions = do
  card <- putIntoPlay p "01008"
  a <- getsGame (toAttrs . fromMaybe (error "fixture upgrade absent") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  let abilities = [windowAbility a idx window typ NoCost (Run $ msgs <> [RanAbility p (UpgradeRef $ upgradeId a) idx [] NoPayment]) | (idx, window, typ, msgs) <- windows]
  withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (ProbeUpgrade a abilities modifiers protected actions)
  pure card

useProbe :: PlayerCard -> Natural -> GameAppT ()
useProbe card idx = do
  waitForProbe
  choose "use the engine fixture ability" $ \case
    UseAbility a -> abilitySource a == UpgradeRef (upgradeIdOf card) && abilityIndex a == idx
    _ -> False
 where
  waitForProbe = asked >>= \case
    Just (_, cs) | any (isAbilityOfCard card) cs -> pure ()
    Just (_, cs) | any isNoResponse cs -> choose "decline an unrelated response" isNoResponse >> waitForProbe
    _ -> pure ()

useBasicAttack :: IdentityId -> GameAppT ()
useBasicAttack p = choose "make a basic hero attack" $ \case
  UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 301
  _ -> False

useAllyAttack :: AllyId -> GameAppT ()
useAllyAttack aid = choose "make a basic ally attack" $ \case
  UseAbility a -> abilitySource a == AllyRef aid && abilityIndex a == 301
  _ -> False

replaceHandCard :: IdentityId -> PlayerCard -> GameAppT ()
replaceHandCard p card = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityHand = Hand (card : filter ((/= pcCardId card) . pcCardId) (unHand a.playerIdentityHand))}

spec :: Spec
spec = describe "Valkyrie shared engine mechanics" do
  it "an exhausted hero can be declared defender of another player's attack without exhausting again" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        defender = player2 tg
    inGame tg do
      resolve [IdentityMessage p (ChangedToForm A), IdentityMessage defender (ChangedToForm A), IdentityMessage defender ExhaustedIdentity]
      vid <- villainIdNow
      probe <- installProbe defender [(1, W.EnemyAttacked W.When AnyEnemy AnyIdentity, Interrupt, [IdentityMessage defender $ SideMessage $ DefendedWithoutExhausting 0 $ EnemyVillainId vid])] [] Nothing []
      runNow [VillainMessage vid (VillainAttacksWithoutBoost p)]
      useProbe probe 1
      pass
      damageOf p `shouldReturn` 0
      damageOf defender `shouldReturn` 1
      exhaustedNow defender `shouldReturn` True
      -- The ordinary defender question must not reopen after the card's declaration.
      offers isNoDefender `shouldReturn` False

  it "a ready hero declared without exhaustion stays ready" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      vid <- villainIdNow
      probe <- installProbe p [(1, W.EnemyAttacked W.When AnyEnemy AnyIdentity, Interrupt, [IdentityMessage p $ SideMessage $ DefendedWithoutExhausting 0 $ EnemyVillainId vid])] [] Nothing []
      runNow [VillainMessage vid (VillainAttacksWithoutBoost p)]
      useProbe probe 1
      pass
      exhaustedNow p `shouldReturn` False
      damageOf p `shouldReturn` 0

  it "an attack-target increment changes damage only against the marked enemy" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      marked <- addMinion p Minions.titania
      unmarked <- addMinion p Minions.abomination
      _ <- installProbe p [] [(IdentityRef p, [AttackModifier 1, AttackModifierAgainst (EnemyMinionId marked) 1])] Nothing []
      useBasicAttack p
      choose "attack the marked minion" (isTargetChoice marked)
      pass
      minionDamageOf marked `shouldReturn` Just 4
      resolve [IdentityMessage p ReadiedIdentity]
      useBasicAttack p
      choose "attack the unmarked minion" (isTargetChoice unmarked)
      pass
      minionDamageOf unmarked `shouldReturn` Just 3

  it "a defense-target increment reduces only the marked attack" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      marked <- addMinion p Minions.hydraMercenary
      unmarked <- addMinion p Minions.hydraMercenary
      probe <- installProbe p
        [(1, W.EnemyAttacked W.When AnyEnemy AnyIdentity, Interrupt, [])]
        [(IdentityRef p, [DefenseModifier 1, DefenseModifierAgainst (EnemyMinionId marked) 1]), (MinionRef marked, [AttackModifier 5]), (MinionRef unmarked, [AttackModifier 5])] Nothing []
      -- Standard basic defense: Spider-Man DEF3, plus baseline1 and marked1.
      runNow [MinionMessage marked (MinionAttacks p)]
      choose "decline fixture's unused response" isNoResponse
      choose "defend against the marked minion" $ \case Defend (EnemyMinionId m) -> m == marked; _ -> False
      pass
      damageOf p `shouldReturn` 1
      resolve [IdentityMessage p ReadiedIdentity]
      runNow [MinionMessage unmarked (MinionAttacks p)]
      choose "decline fixture's unused response" isNoResponse
      choose "defend against the unmarked minion" $ \case Defend (EnemyMinionId m) -> m == unmarked; _ -> False
      pass
      damageOf p `shouldReturn` 3
      offers (isAbilityOfCard probe) `shouldReturn` False

  for_ [False, True] $ \expertFirst ->
    it ("using ATK instead of DEF ignores DEF bonuses in either response order: Expert Defense first = " <> show expertFirst) do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        attacker <- addMinion p Minions.hydraMercenary
        probe <- installProbe p [(1, W.HeroDefended W.When (IdentityWithId p) AnyEnemy, Interrupt, [])]
          [(IdentityRef p, [AttackModifier 1, DefenseModifier 4]), (MinionRef attacker, [AttackModifier 5])] Nothing [(1, ReplaceDefenseWithAttack p (EnemyMinionId attacker))]
        expert <- putInHand p "03033"
        runNow [MinionMessage attacker (MinionAttacks p)]
        choose "make the basic defense" $ \case Defend (EnemyMinionId m) -> m == attacker; _ -> False
        if expertFirst
          then playWith expert [] >> useProbe probe 1
          else useProbe probe 1 >> playWith expert []
        pass
        -- Incoming6 minus modifiedATK3, ignoring DEF3+4+ExpertDefense3.
        damageOf p `shouldReturn` 3

  it "set-aside play keeps the original card identity and pays its normal resource cost" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      card <- putInHand p "01057"
      resolve [SetAside [PlayerCard card]]
      handOf p >>= (`shouldSatisfy` notElem card)
      getSetAsidePlayerCards >>= (`shouldSatisfy` elem card)
      selectList (AffordableCardBy (IdentityWithId p) $ InSetAsideOf (IdentityWithId p) $ BasicCardMatches $ CardIs Upgrades.combatTraining) `shouldReturn` []
      money <- putInHand p "01088"
      runNow [IdentityMessage p $ PlayingCard card Nothing]
      offers (isPayWith money) `shouldReturn` True
      payWith [money]
      pass
      live <- upgradesWith "01057"
      map upgradeId live `shouldBe` [upgradeIdOf card]
      map upgradeOwner live `shouldBe` [p]
      getSetAsidePlayerCards >>= (`shouldSatisfy` notElem card)
      discardOf p >>= (`shouldSatisfy` elem money)

  it "an identity-specific enemy attack prohibition allows its allies to attack that enemy" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      marked <- addMinion p Minions.hydraMercenary
      unmarked <- addMinion p Minions.hydraMercenary
      ally <- putIntoPlay p "01051"
      _ <- installProbe p [] [(IdentityRef p, [CannotAttackEnemy $ EnemyMinionId marked])] Nothing []
      heroTargets <- selectList (AttackableBy $ IdentityWithId p)
      allyTargets <- selectList (AttackableByAlly $ allyIdOf ally)
      heroTargets `shouldSatisfy` notElem (EnemyMinionId marked)
      heroTargets `shouldSatisfy` elem (EnemyMinionId unmarked)
      allyTargets `shouldSatisfy` elem (EnemyMinionId marked)

  it "allies retain guard restrictions without inheriting the hero's specific attack prohibition" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      guard <- addMinion p Minions.hydraMercenary
      vid <- villainIdNow
      ally <- putIntoPlay p "01051"
      _ <- installProbe p [] [(IdentityRef p, [IgnoresGuard, CannotAttackEnemy $ EnemyVillainId vid])] Nothing []
      heroTargets <- selectList (AttackableBy $ IdentityWithId p)
      allyTargets <- selectList (AttackableByAlly $ allyIdOf ally)
      heroTargets `shouldSatisfy` notElem (EnemyVillainId vid)
      allyTargets `shouldSatisfy` notElem (EnemyVillainId vid)
      resolve [RemoveFromPlay $ MinionRef guard]
      afterGuard <- selectList (AttackableByAlly $ allyIdOf ally)
      afterGuard `shouldSatisfy` elem (EnemyVillainId vid)

  it "a defeat response sees the attached-upgrade snapshot after its interrupt sets that upgrade aside" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      marked <- addMinion p Minions.hydraMercenary
      marker <- putIntoPlay p "01057"
      resolve [UpgradeMessage (upgradeIdOf marker) $ UpgradeAttachedToEnemy $ EnemyMinionId marked]
      probe <- installProbe p
        [(1, W.EnemyDefeatedWithUpgrade W.When Upgrades.combatTraining AnyDamage, Interrupt, [SetAside [PlayerCard marker]])
        ,(2, W.EnemyDefeatedWithUpgrade W.After Upgrades.combatTraining AnyDamage, Response, [PlaceCounters (IdentityRef p) 1])] [] Nothing []
      runNow [MinionMessage marked $ MinionDamaged (IdentityRef p) $ toDamage 3 FromAbility]
      useProbe probe 1
      upgradesWith "01057" `shouldReturn` []
      getSetAsidePlayerCards >>= (`shouldSatisfy` elem marker)
      useProbe probe 2
      pass
      playerIdentityCounters . toAttrs <$> getPlayer p `shouldReturn` 1

  it "one ally attack resolves targets in the chosen order and takes consequential damage once" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01051"
      let aid = allyIdOf ally
      first <- addMinion p Minions.hydraMercenary
      second <- addMinion p Minions.hydraMercenary
      probe <- installProbe p
        [(1, W.AllyAttacked W.When (AllyWithId aid) MinionEnemy, Interrupt, [])
        ,(2, W.AllyAttacked W.After (AllyWithId aid) AnyEnemy, Response, [PlaceCounters (IdentityRef p) 1])] [] Nothing [(1, BroadenAttack aid [EnemyMinionId first, EnemyMinionId second])]
      useAllyAttack aid
      choose "initial target" (isTargetChoice first)
      useProbe probe 1
      choose "resolve the second minion first" (isTargetChoice second)
      minionDamageOf second `shouldReturn` Just 2
      minionDamageOf first `shouldReturn` Just 0
      choose "then resolve the first minion" (isTargetChoice first)
      useProbe probe 2
      pass
      getsGame (fmap (Ally.allyDamage . toAttrs) . HashMap.lookup aid . gameAllies) `shouldReturn` Just 1
      playerIdentityCounters . toAttrs <$> getPlayer p `shouldReturn` 1

  it "a minion-search cost finishes quickstrike before the event starts healing" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      setDamage p 1
      original <- putInHand p "01086"
      let card = original {pcCardDef = (getCardDef original) {cdCost = Just 0, cdAbilityCost = SearchEncounterMinionCost}}
      replaceHandCard p card
      (deck, _) <- setEncounterDeck [Minions.vulture, Schemes.bombScare] []
      playWith card []
      choose "search for Vulture" (isTargetChoice $ testHead deck)
      -- Quickstrike's defender choice is still part of paying the cost.
      damageOf p `shouldReturn` 1
      choose "do not defend quickstrike" isNoDefender
      pass
      -- First Aid cannot choose its target until Vulture's whole attack resolved.
      damageOf p `shouldReturn` 4
      choose "heal after paying the search cost" (\choice -> isTargetChoice p choice && case choice of TargetLabel _ cs -> not (null cs); _ -> False)
      pass
      damageOf p `shouldReturn` 2

  it "a deck-only scheme-search cost reveals the scheme before the event effect" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      setDamage p 1
      original <- putInHand p "01086"
      let card = original {pcCardDef = (getCardDef original) {cdCost = Just 0, cdAbilityCost = SearchEncounterSideSchemeCost}}
      replaceHandCard p card
      (deck, discard) <- setEncounterDeck [Schemes.bombScare] [Schemes.crowdControl]
      playWith card []
      offers (isTargetChoice $ testHead discard) `shouldReturn` False
      choose "reveal the scheme from the deck" (isTargetChoice $ testHead deck)
      pass
      threats <- getsGame (map (Scheme.getSideSchemeThreat) . HashMap.elems . gameSideSchemes)
      threats `shouldBe` [3]
      damageOf p `shouldReturn` 1
      choose "now resolve First Aid" (isTargetChoice p)
      pass
      damageOf p `shouldReturn` 0

  it "an attachment's discard action is unavailable while protected, then becomes usable" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      seduced <- encounterCard Attachments.seduced
      resolve [RevealEncounterCard p seduced]
      let aid = AttachmentId $ unCardId $ ecCardId seduced
          discards = \case UseAbility a -> abilitySource a == AttachmentRef aid; _ -> False
      probe <- installProbe p [] [] (Just aid) []
      energy <- putInHand p "01088"
      mental <- putInHand p "01089"
      offers discards `shouldReturn` False
      resolve [RemoveFromPlay $ AttachmentRef aid]
      getsGame (HashMap.member aid . gameAttachments) `shouldReturn` True
      resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf probe]
      offers discards `shouldReturn` True
      choose "pay to discard Seduced" discards
      payWith [energy, mental]
      pass
      getsGame (HashMap.member aid . gameAttachments) `shouldReturn` False

  it "a protected attachment still leaves when its friendly ally host leaves play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01051"
      seduced <- encounterCard Attachments.seduced
      resolve [RevealEncounterCard p seduced]
      let aid = AttachmentId $ unCardId $ ecCardId seduced
      resolve [AttachmentMessage aid $ AttachedToAlly $ allyIdOf ally]
      _ <- installProbe p [] [] (Just aid) []
      resolve [RemoveFromPlay $ AllyRef $ allyIdOf ally]
      getsGame (HashMap.member aid . gameAttachments) `shouldReturn` False
