module Marvel.Hero.ValkyrieSignatureSpec (spec) where

import Data.List (find)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

identityAbility :: IdentityId -> Natural -> Choice -> Bool
identityAbility p idx = \case
  UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == idx
  _ -> False

playGlow :: IsRef enemy => IdentityId -> enemy -> GameAppT PlayerCard
playGlow p enemy = do
  glow <- getSetAsidePlayerCards >>= \cs -> pure $ fromMaybe (error "no set-aside Death-Glow") (find ((== "25002") . toCardCode) cs)
  resource <- putInHand p "25024"
  choose "Death Perception" (identityAbility p 1)
  choose "mark the enemy" (isTargetChoice enemy)
  payWith [resource]
  pass
  pure glow

resolveGlow :: PlayerCard -> GameAppT ()
resolveGlow glow = choose "resolve Death-Glow" $ \choice -> isAbilityOfCard glow choice && case choice of
  UseAbility a -> abilityIndex a == 1
  _ -> False

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code resources = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resources
  playWith card payment
  pass
  pure card

haveAtThee :: IdentityId -> GameAppT PlayerCard
haveAtThee p = do
  card <- putInHand p "25012"
  energy <- putInHand p "25025"
  single <- putInHand p "25024"
  playWith card [energy, single]
  pure card

spec :: Spec
spec = describe "Valkyrie signature events" do
  describe "Visit Valhalla" do
    it "costs0, returns one chosen Valkyrie card from your discard to hand, and leaves the other cards there" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        own <- stackDiscard p ["25003", "25004", "25006", "25012", "25018", "25024"]
        [otherDiscard] <- stackDiscard (player2 tg) ["25007"]
        top <- stackDeck p ["25026"]
        card <- putInHand p "25009"
        resolve [IdentityMessage p ExhaustedIdentity]
        playWith card []
        for_ (take 4 own) $ \candidate -> offers (isTargetChoice candidate) `shouldReturn` True
        for_ (drop 4 own <> [otherDiscard]) $ \candidate -> offers (isTargetChoice candidate) `shouldReturn` False
        choose "return Dragonfang" (isTargetChoice $ own !! 2)
        pass
        handOf p `shouldReturn` [own !! 2]
        discard <- discardOf p
        discard `shouldSatisfy` notElem (own !! 2)
        all (`elem` discard) [head own, own !! 1, own !! 3, own !! 4, own !! 5, card] `shouldBe` True
        take 1 . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` top
        exhaustedNow p `shouldReturn` True

    it "is an alter-ego action, so it cannot be played in hero form" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- stackDiscard p ["25006"]
        card <- putInHand p "25009"
        changeForm p
        offers (isPlay card) `shouldReturn` False

    it "cannot be played with no Valkyrie card in your discard, even if other cards or another player's Valkyrie card are there" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- stackDiscard p ["25018", "25024", "25025"]
        _ <- stackDiscard (player2 tg) ["25007"]
        card <- putInHand p "25009"
        offers (isPlay card) `shouldReturn` False
        handOf p `shouldReturn` [card]

  describe "Chooser of the Slain" do
    it "costs0 and offers minions from deck and discard, putting the chosen one into play engaged with you before drawing2" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        (deck, discard) <- setEncounterDeck [Minions.shocker, Schemes.bombScare] [Minions.hydraMercenary, Treacheries.assault]
        card <- putInHand p "25010"
        resolve [IdentityMessage p ExhaustedIdentity]
        playWith card []
        length <$> handOf p `shouldReturn` 0
        offers (isTargetChoice $ head deck) `shouldReturn` True
        offers (isTargetChoice $ head discard) `shouldReturn` True
        offers (isTargetChoice $ deck !! 1) `shouldReturn` False
        offers (isTargetChoice $ discard !! 1) `shouldReturn` False
        choose "put Shocker into play" (isTargetChoice $ head deck)
        pass
        let mid = MinionId $ unCardId $ ecCardId $ head deck
        getsGame (fmap (minionEngagedIdentity . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just p
        length <$> handOf p `shouldReturn` 2
        -- Put into play does not reveal him: Shocker's When Revealed deals no damage.
        damageOf p `shouldReturn` 0
        exhaustedNow p `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem card)

    it "may choose a minion from discard when the encounter deck contains none" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        (_, discard) <- setEncounterDeck [Schemes.bombScare] [Minions.hydraMercenary]
        card <- putInHand p "25010"
        playWith card []
        choose "take the discarded minion" (isTargetChoice $ head discard)
        pass
        let mid = MinionId $ unCardId $ ecCardId $ head discard
        getsGame (HashMap.member mid . gameMinions) `shouldReturn` True
        encounterDiscard >>= (`shouldSatisfy` notElem (head discard))
        length <$> handOf p `shouldReturn` 2

    it "finishes a searched minion's quickstrike attack before drawing its2 cards" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        (deck, _) <- setEncounterDeck [Minions.vulture, Schemes.bombScare] []
        card <- putInHand p "25010"
        playWith card []
        choose "search for Vulture" (isTargetChoice $ head deck)
        length <$> handOf p `shouldReturn` 0
        choose "do not defend quickstrike" isNoDefender
        pass
        damageOf p `shouldReturn` 3
        length <$> handOf p `shouldReturn` 2

    it "does not draw when the selected unique minion is prevented from entering play" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        existing <- addMinion p Minions.shocker
        changeForm p
        clearHand p
        top <- stackDeck p ["25026", "25027"]
        (deck, _) <- setEncounterDeck [Minions.shocker, Schemes.bombScare] []
        card <- putInHand p "25010"
        playWith card []
        choose "attempt to put another unique Shocker into play" (isTargetChoice $ head deck)
        pass
        hand <- handOf p
        all (`notElem` hand) top `shouldBe` True
        take 2 . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` top
        getsGame (HashMap.keys . gameMinions) `shouldReturn` [existing]

    it "is not playable in alter-ego form" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Minions.hydraMercenary] []
        card <- putInHand p "25010"
        offers (isPlay card) `shouldReturn` False

    it "cannot draw cards when no minion can be found to pay the cost" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        _ <- setEncounterDeck [Schemes.bombScare] [Treacheries.assault]
        card <- putInHand p "25010"
        offers (isPlay card) `shouldReturn` False
        handOf p `shouldReturn` [card]

  describe "Shieldmaiden" do
    for_ [False, True] $ \startsExhausted ->
      it ("redirects a marked enemy's attack from another player without exhausting Valkyrie; already exhausted = " <> show startsExhausted) do
        tg <- testGame ["valkyrie", "spider-man"]
        let p = player1 tg
            other = player2 tg
        inGame tg do
          attacker <- addMinion other Minions.tigerShark
          changeForm p
          _ <- playGlow p attacker
          clearHand p
          shield <- putInHand p "25011"
          resource <- putInHand p "25024"
          when startsExhausted $ resolve [IdentityMessage p ExhaustedIdentity]
          runNow [MinionMessage attacker $ MinionAttacks other]
          playWith shield [resource]
          pass
          -- 3ATK versus Valkyrie's 1DEF plus Shieldmaiden's2.
          damageOf p `shouldReturn` 0
          damageOf other `shouldReturn` 0
          exhaustedNow p `shouldReturn` startsExhausted
          offers isNoDefender `shouldReturn` False
          discardOf p >>= (`shouldSatisfy` elem shield)

    it "gives +2DEF for this attack only, so the next ordinary defense uses the printed DEF" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        attacker <- addMinion p Minions.tigerShark
        changeForm p
        _ <- playGlow p attacker
        clearHand p
        shield <- putInHand p "25011"
        resource <- putInHand p "25024"
        runNow [MinionMessage attacker $ MinionAttacks p]
        playWith shield [resource]
        pass
        damageOf p `shouldReturn` 0
        -- Tiger Shark's forced response gives it tough; resolve it before attacking again.
        chooseIf isAnyAbility
        pass
        runNow [MinionMessage attacker $ MinionAttacks p]
        choose "make the next ordinary defense" $ \case Defend (EnemyMinionId mid) -> mid == attacker; _ -> False
        pass
        damageOf p `shouldReturn` 2
        exhaustedNow p `shouldReturn` True

    it "combines its +2DEF with Spear's marked-enemy +2, rather than its ordinary +1" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- playPaying p "25005" ["25024"]
        attacker <- addMinion p Minions.titania
        resolve [MinionMessage attacker $ MinionDamaged GameRef $ toDamage 1 FromAbility]
        changeForm p
        _ <- playGlow p attacker
        clearHand p
        shield <- putInHand p "25011"
        resource <- putInHand p "25024"
        runNow [MinionMessage attacker $ MinionAttacks p]
        playWith shield [resource]
        pass
        -- Titania's5 ATK versus 1+2+2 DEF.
        damageOf p `shouldReturn` 0
        exhaustedNow p `shouldReturn` False
        currentIdentity <$> getPlayer p >>= \case
          HeroSide h -> Hero.getModifiedDefense (toAttrs h) `shouldReturn` 2
          _ -> error "expected Valkyrie"

    it "using The Best Defense with Shieldmaiden ignores both Spear's DEF and Shieldmaiden's DEF, using Dragonfang's normal ATK bonus" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- playPaying p "25005" ["25024"]
        _ <- playPaying p "25006" ["25024"]
        attacker <- addMinion p Minions.titania
        resolve [MinionMessage attacker $ MinionDamaged GameRef $ toDamage 1 FromAbility]
        changeForm p
        _ <- playGlow p attacker
        clearHand p
        shield <- putInHand p "25011"
        best <- putInHand p "25020"
        resource <- putInHand p "25024"
        runNow [MinionMessage attacker $ MinionAttacks p]
        playWith shield [resource]
        playWith best []
        pass
        -- The card is used while defending, so Dragonfang grants1, not its attacking2.
        damageOf p `shouldReturn` 2
        exhaustedNow p `shouldReturn` False

    it "cannot be used against an unmarked enemy's attack" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        marked <- addMinion p Minions.titania
        other <- addMinion p Minions.hydraMercenary
        changeForm p
        _ <- playGlow p marked
        shield <- putInHand p "25011"
        _ <- putInHand p "25024"
        runNow [MinionMessage other $ MinionAttacks p]
        offers (isPlay shield) `shouldReturn` False
        pass
        discardOf p >>= (`shouldSatisfy` notElem shield)

    it "cannot be used in alter-ego form" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        attacker <- addMinion p Minions.hydraMercenary
        changeForm p
        _ <- playGlow p attacker
        resolve [IdentityMessage p $ ChangedToForm B]
        shield <- putInHand p "25011"
        _ <- putInHand p "25024"
        runNow [MinionMessage attacker $ MinionAttacks p]
        offers (isPlay shield) `shouldReturn` False
        pass

    it "requires paying its1 resource even when the enemy is marked" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        attacker <- addMinion p Minions.hydraMercenary
        changeForm p
        _ <- playGlow p attacker
        clearHand p
        shield <- putInHand p "25011"
        runNow [MinionMessage attacker $ MinionAttacks p]
        offers (isPlay shield) `shouldReturn` False
        pass

  describe "Have at Thee!" do
    it "costs3 and deals7 damage directly to the villain" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        card <- haveAtThee p
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 7
        discardOf p >>= (`shouldSatisfy` elem card)

    it "gains overkill against the marked minion, preserving the overflow even after Death-Glow is set aside" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.hydraMercenary
        changeForm p
        glow <- playGlow p mid
        clearHand p
        _ <- haveAtThee p
        choose "attack the marked3HP minion" (isTargetChoice mid)
        resolveGlow glow
        pass
        minionDamageOf mid `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 4
        getSetAsidePlayerCards >>= (`shouldSatisfy` elem glow)

    it "does not gain overkill against an unmarked minion even while another enemy has Death-Glow" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        marked <- addMinion p Minions.titania
        target <- addMinion p Minions.hydraMercenary
        changeForm p
        _ <- playGlow p marked
        clearHand p
        _ <- haveAtThee p
        choose "attack the unmarked3HP minion" (isTargetChoice target)
        pass
        minionDamageOf target `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0
        length <$> upgradesWith "25002" `shouldReturn` 1

    it "respects guard when choosing its attack target" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        guard <- addMinion p Minions.hydraMercenary
        changeForm p
        clearHand p
        _ <- haveAtThee p
        vid <- villainIdNow
        offers (isTargetChoice vid) `shouldReturn` False
        choose "attack the guard instead" (isTargetChoice guard)
        pass
        villainDamageNow `shouldReturn` 0

    it "a tough marked target prevents the attack, including any overkill" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.hydraMercenary
        changeForm p
        _ <- playGlow p mid
        resolve [GiveStatus (MinionRef mid) ToughStatus]
        clearHand p
        _ <- haveAtThee p
        choose "attack the tough marked minion" (isTargetChoice mid)
        pass
        minionDamageOf mid `shouldReturn` Just 0
        villainDamageNow `shouldReturn` 0
        getsGame (fmap (Minion.minionTough . toAttrs) . HashMap.lookup mid . gameMinions) `shouldReturn` Just False
        length <$> upgradesWith "25002" `shouldReturn` 1

    it "pays its cost but only removes stun when Valkyrie is stunned" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        card <- haveAtThee p
        pass
        villainDamageNow `shouldReturn` 0
        playerIdentityStunned . toAttrs <$> getPlayer p `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem card)
        length <$> handOf p `shouldReturn` 0

    it "is not playable in alter-ego form or without3 resources" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "25012"
        energy <- putInHand p "25025"
        _ <- putInHand p "25024"
        offers (isPlay card) `shouldReturn` False
        changeForm p
        resolve [RemoveFromGame $ toRef energy]
        offers (isPlay card) `shouldReturn` False
