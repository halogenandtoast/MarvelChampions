module Marvel.Hero.WolverineSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity, PlayerIdentitySide (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Phase
import Marvel.Status
import Marvel.Window qualified as W
import TestImport

asWolverine :: IdentityId -> GameAppT ()
asWolverine p = changeForm p >> pass >> clearHand p

clawsCard :: IdentityId -> GameAppT PlayerCard
clawsCard p = do
  claws <- upgradesWith "35002"
  case filter ((== p) . upgradeController) claws of
    [a] -> pure $ MkPlayerCard (CardId $ unUpgradeId $ upgradeId a) (lookupPlayerCard "35002") (Just p) (Just p)
    _ -> error "Wolverine's Claws not in play"

payEvent :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
payEvent p code resources = do
  c <- putInHand p code
  payments <- traverse (putInHand p) resources
  playWith c payments
  pure c

useClaws :: IdentityId -> PlayerCard -> GameAppT ()
useClaws p event = do
  claws <- clawsCard p
  choose "Wolverine's Claws" (isAbilityOfCard claws)
  choose "the attack event" (isTargetChoice event)

makeVillainTough :: GameAppT ()
makeVillainTough = villainIdNow >>= \vid -> resolve [GiveStatus (VillainRef vid) ToughStatus]

spec :: Spec
spec = describe "Wolverine" do
  it "Logan sets permanent Claws in play and starts with REC 6 and hand size 6" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      _ <- clawsCard p
      hitPointsOf p `shouldReturn` 10
      handSizeOf p `shouldReturn` 6
      setDamage p 8
      choose "basic recovery" \case
        UseAbility ab -> Recover `elem` abilityChoices ab && abilitySource ab == IdentityRef p
        _ -> False
      damageOf p `shouldReturn` 2
      exhaustedNow p `shouldReturn` True

  it "Healing Factor heals two at the start of the player phase" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      setDamage p 5
      start [CheckWindows [W.Window W.After $ W.PhaseBegan PlayerPhase]]
      choose "Healing Factor" (isNamedAbility "$label.healingFactor")
      damageOf p `shouldReturn` 3

  it "Healing Factor is absent in alter ego and at full health" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      setDamage p 3
      start [CheckWindows [W.Window W.After $ W.PhaseBegan PlayerPhase]]
      offers (isNamedAbility "$label.healingFactor") `shouldReturn` False
      pass
      asWolverine p
      setDamage p 0
      start [CheckWindows [W.Window W.After $ W.PhaseBegan PlayerPhase]]
      offers (isNamedAbility "$label.healingFactor") `shouldReturn` False

  it "Claws pays printed damage and plays an attack without resources, piercing tough" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      card <- putInHand p "35008"
      makeVillainTough
      useClaws p card
      damageOf p `shouldReturn` 2
      chooseVillain
      villainDamageNow `shouldReturn` 4
      claws <- clawsCard p
      offers (isAbilityOfCard claws) `shouldReturn` False

  it "Claws is unavailable with tough, without an Attack event, or in alter ego" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- putInHand p "35010"
      claws <- clawsCard p
      offers (isAbilityOfCard claws) `shouldReturn` False
      asWolverine p
      _ <- putInHand p "35011"
      offers (isAbilityOfCard claws) `shouldReturn` False
      _ <- putInHand p "35010"
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      offers (isAbilityOfCard claws) `shouldReturn` False

  it "Claws does not damage the hero for a zero-cost Attack event" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      card <- putInHand p "06014"
      useClaws p card
      damageOf p `shouldReturn` 0
      choose "minion" (isTargetChoice minion)
      minionDamageOf minion `shouldReturn` Just 1

  it "Lunging Strike deals eight without overkill when paid normally" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      _ <- payEvent p "35010" ["01088", "01089"]
      choose "minion" (isTargetChoice minion)
      minionDamageOf minion `shouldReturn` Nothing
      villainDamageNow `shouldReturn` 0

  it "Claws gives Lunging Strike piercing and overkill" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      card <- putInHand p "35010"
      useClaws p card
      choose "minion" (isTargetChoice minion)
      minionDamageOf minion `shouldReturn` Nothing
      villainDamageNow `shouldReturn` 5
      damageOf p `shouldReturn` 3

  it "Slice and Dice makes two independently chosen attacks" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      _ <- payEvent p "35009" ["01088", "01089"]
      choose "first attack" (isTargetChoice minion)
      chooseVillain
      minionDamageOf minion `shouldReturn` Nothing
      villainDamageNow `shouldReturn` 3

  it "Slice and Dice consumes stun on only its first listed attack" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      resolve [GiveStatus (IdentityRef p) StunnedStatus]
      _ <- payEvent p "35009" ["01088", "01089"]
      chooseVillain
      villainDamageNow `shouldReturn` 3
      isStunned p `shouldReturn` False

  it "Claws piercing survives a stun-replaced first Slice and Dice attack" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      resolve [GiveStatus (IdentityRef p) StunnedStatus]
      makeVillainTough
      card <- putInHand p "35009"
      useClaws p card
      chooseVillain
      villainDamageNow `shouldReturn` 3

  it "Claws piercing is spent by the first actual Slice and Dice attack" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      makeVillainTough
      card <- putInHand p "35009"
      useClaws p card
      choose "first attack" (isTargetChoice minion)
      chooseVillain
      villainDamageNow `shouldReturn` 0

  it "Berserker Barrage can take two damage to repeat after defeating an enemy" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      _ <- payEvent p "35008" ["01088"]
      choose "minion" (isTargetChoice minion)
      choose "repeat" (isLabel "$label.berserkerBarrageRepeat")
      chooseVillain
      damageOf p `shouldReturn` 2
      villainDamageNow `shouldReturn` 4

  it "Berserker Barrage offers no repeat without defeating an enemy" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      _ <- payEvent p "35008" ["01088"]
      chooseVillain
      offers (isLabel "$label.berserkerBarrageRepeat") `shouldReturn` False
      damageOf p `shouldReturn` 0

  it "Claws piercing is lost on a repeated Berserker Barrage" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      makeVillainTough
      card <- putInHand p "35008"
      useClaws p card
      choose "minion" (isTargetChoice minion)
      choose "repeat" (isLabel "$label.berserkerBarrageRepeat")
      chooseVillain
      villainDamageNow `shouldReturn` 0
      damageOf p `shouldReturn` 4

  it "Track by Scent removes three threat and draws two for the last threat" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      placeMainThreat 3
      _ <- payEvent p "35011" ["01088"]
      before <- length <$> handOf p
      main <- mainSchemeIdNow
      choose "main scheme" (isTargetChoice main)
      mainSchemeThreatNow `shouldReturn` 0
      (length <$> handOf p) `shouldReturn` (before + 2)

  it "Track by Scent draws nothing if threat remains" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      placeMainThreat 5
      beforeThreat <- mainSchemeThreatNow
      _ <- payEvent p "35011" ["01088"]
      before <- length <$> handOf p
      main <- mainSchemeIdNow
      choose "main scheme" (isTargetChoice main)
      mainSchemeThreatNow `shouldReturn` (beforeThreat - 3)
      (length <$> handOf p) `shouldReturn` before

  it "Regenerative Healing heals four in alter ego" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      clearHand p
      setDamage p 6
      _ <- payEvent p "35012" ["01088"]
      choose "heal four" (isLabel $ countLabel "healYourIdentity" 4)
      damageOf p `shouldReturn` 2

  it "Regenerative Healing discards both status types instead of healing" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      setDamage p 5
      resolve [GiveStatus (IdentityRef p) StunnedStatus, GiveStatus (IdentityRef p) ConfusedStatus]
      _ <- payEvent p "35012" ["01088"]
      choose "discard statuses" (isLabel "$label.regenerativeHealingStatuses")
      isStunned p `shouldReturn` False
      isConfused p `shouldReturn` False
      damageOf p `shouldReturn` 5

  it "Regenerative Healing is unavailable with neither damage nor status cards" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "35012"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False

  it "Adamantium Skeleton grants four hit points in either form and one hero ATK" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "35004"
      hitPointsOf p `shouldReturn` 14
      asWolverine p
      hitPointsOf p `shouldReturn` 14
      currentIdentity <$> getPlayer p >>= \case
        HeroSide h -> Hero.getModifiedAttack (toAttrs h) `shouldReturn` 3
        _ -> error "expected hero"
      makeVillainTough
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      villainDamageNow `shouldReturn` 3

  it "Adamantium Skeleton does not give piercing to events" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      _ <- putIntoPlay p "35004"
      makeVillainTough
      _ <- payEvent p "35008" ["01088"]
      chooseVillain
      villainDamageNow `shouldReturn` 0

  it "Berserker Frenzy draws after enemy attack damage and discards on flipping to alter ego" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      frenzy <- putIntoPlay p "35005"
      vid <- villainIdNow
      before <- length <$> handOf p
      start (damageIdentity p (VillainRef vid) $ toDamage 2 FromAttack)
      choose "Berserker Frenzy" (isAbilityOfCard frenzy)
      (length <$> handOf p) `shouldReturn` (before + 1)
      runNow [IdentityMessage p (ChangedToForm B)]
      choose "resolve Berserker Frenzy's forced discard" (isAbilityOfCard frenzy)
      pass
      discardOf p >>= (`shouldSatisfy` elem frenzy)

  it "Berserker Frenzy does not answer damage from Claws or a prevented attack" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      frenzy <- putIntoPlay p "35005"
      start (damageIdentity p GameRef $ toDamage 1 FromAbility)
      offers (isAbilityOfCard frenzy) `shouldReturn` False
      pass
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      vid <- villainIdNow
      start (damageIdentity p (VillainRef vid) $ toDamage 2 FromAttack)
      offers (isAbilityOfCard frenzy) `shouldReturn` False

  it "I Got Better replaces lethal enemy attack damage, readies and discards" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      better <- putIntoPlay p "35006"
      setDamage p 8
      runNow [IdentityMessage p ExhaustedIdentity]
      vid <- villainIdNow
      start (damageIdentity p (VillainRef vid) $ toDamage 3 FromAttack)
      choose "I Got Better" (isAbilityOfCard better)
      damageOf p `shouldReturn` 5
      exhaustedNow p `shouldReturn` False
      (.playerIdentityDefeated) <$> playerAttrs p >>= (`shouldBe` False)
      discardOf p >>= (`shouldSatisfy` elem better)

  it "I Got Better is not offered for lethal nonattack damage" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      better <- putIntoPlay p "35006"
      setDamage p 8
      start (damageIdentity p GameRef $ toDamage 3 FromAbility)
      offers (isAbilityOfCard better) `shouldReturn` False

  it "Logan's Cabin shuffles a Wolverine card and exhausts" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      cabin <- putIntoPlay p "35007"
      signature <- putInDiscard p "35010"
      other <- putInDiscard p "01090"
      choose "Logan's Cabin" (isAbilityOfCard cabin)
      offers (isTargetChoice other) `shouldReturn` False
      choose "Wolverine card" (isTargetChoice signature)
      discardOf p >>= (`shouldSatisfy` notElem signature)
      (unDeck . (.playerIdentityDeck) <$> playerAttrs p) >>= (`shouldSatisfy` elem signature)
      offers (isAbilityOfCard cabin) `shouldReturn` False

  it "Jubilee grants Wolverine two ATK only against the chosen enemy" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      card <- putInHand p "35003"
      energy <- putInHand p "01088"
      playWith card [energy]
      choose "Jubilee" (isAbilityOfCard card)
      chooseVillain
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      villainDamageNow `shouldReturn` 4

  it "Jubilee's own basic attack gets the bonus against the chosen enemy" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      card <- putInHand p "35003"
      energy <- putInHand p "01088"
      playWith card [energy]
      choose "Jubilee" (isAbilityOfCard card)
      chooseVillain
      a <- getsGame (toAttrs . fromMaybe (error "missing Jubilee") . HashMap.lookup (allyIdOf card) . gameAllies)
      choose "Jubilee attack" (isAllyAttack a)
      chooseVillain
      villainDamageNow `shouldReturn` 3

  it "Jubilee does not increase an attack against another enemy" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      card <- putInHand p "35003"
      energy <- putInHand p "01088"
      playWith card [energy]
      choose "Jubilee" (isAbilityOfCard card)
      chooseVillain
      choose "basic attack" (isHeroAttack p)
      choose "other enemy" (isTargetChoice minion)
      minionDamageOf minion `shouldReturn` Just 2

  it "Jubilee's bonus does not add damage to an Attack event" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      card <- putInHand p "35003"
      energy <- putInHand p "01088"
      playWith card [energy]
      choose "Jubilee" (isAbilityOfCard card)
      chooseVillain
      _ <- payEvent p "35008" ["01088"]
      chooseVillain
      villainDamageNow `shouldReturn` 4

  it "Berserker Barrage can decline the optional repeat without damage" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      _ <- payEvent p "35008" ["01088"]
      choose "minion" (isTargetChoice minion)
      choose "decline repeat" (isLabel "$label.done")
      damageOf p `shouldReturn` 0
      villainDamageNow `shouldReturn` 0

  it "Logan's Cabin is unavailable in hero form or with no Wolverine discard" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      cabin <- putIntoPlay p "35007"
      offers (isAbilityOfCard cabin) `shouldReturn` False
      _ <- putInDiscard p "01090"
      offers (isAbilityOfCard cabin) `shouldReturn` False
      _ <- putInDiscard p "35010"
      asWolverine p
      offers (isAbilityOfCard cabin) `shouldReturn` False

  it "Berserker Barrage does not repeat if tough prevents the required damage" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      _ <- payEvent p "35008" ["01088"]
      choose "minion" (isTargetChoice minion)
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      choose "try repeating" (isLabel "$label.berserkerBarrageRepeat")
      damageOf p `shouldReturn` 0
      villainDamageNow `shouldReturn` 0
      vid <- villainIdNow
      offers (isTargetChoice vid) `shouldReturn` False

  it "Regenerative Healing can discard statuses at full health without offering healing" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      clearHand p
      resolve [GiveStatus (IdentityRef p) ConfusedStatus]
      _ <- payEvent p "35012" ["01088"]
      offers (isLabel $ countLabel "healYourIdentity" 4) `shouldReturn` False
      choose "discard confused" (isLabel "$label.regenerativeHealingStatuses")
      isConfused p `shouldReturn` False
      damageOf p `shouldReturn` 0

  it "Berserker Frenzy responds to overkill damage from an enemy attack" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      frenzy <- putIntoPlay p "35005"
      vid <- villainIdNow
      before <- length <$> handOf p
      start (damageIdentity p (VillainRef vid) $ toDamage 2 FromOverkill)
      choose "Berserker Frenzy" (isAbilityOfCard frenzy)
      (length <$> handOf p) `shouldReturn` (before + 1)

  it "Claws gives piercing to every damage instance of Melee's single attack" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      resolve [GiveStatus (MinionRef minion) ToughStatus]
      makeVillainTough
      card <- putInHand p "05030"
      useClaws p card
      choose "first enemy" (isTargetChoice minion)
      chooseVillain
      minionDamageOf minion `shouldReturn` Nothing
      villainDamageNow `shouldReturn` 3
      damageOf p `shouldReturn` 3

  it "Claws aborts the attack event when Side Step prevents its damage payment" do
    tg <- testGame ["wolverine"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      strike <- putInHand p "35010"
      sideStep <- putInHand p "14015"
      genius <- putInHand p "01089"
      useClaws p strike
      playWith sideStep [genius]
      resolve []
      damageOf p `shouldReturn` 0
      villainDamageNow `shouldReturn` 0
      handOf p >>= (`shouldSatisfy` elem strike)
      discardOf p >>= (`shouldSatisfy` elem sideStep)
      claws <- clawsCard p
      upgradeIsExhausted (upgradeIdOf claws) `shouldReturn` True
      vid <- villainIdNow
      offers (isTargetChoice vid) `shouldReturn` False

  it "Slice and Dice cannot make its second attack after retaliation eliminates Wolverine" do
    tg <- testGame ["wolverine", "captain-marvel"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      whiplash <- addMinion p Minions.whiplash
      setDamage p 9
      event <- payEvent p "35009" ["01088", "01089"]
      choose "first attack on Whiplash" (isTargetChoice whiplash)
      getsGame (HashMap.member p . gamePlayers) `shouldReturn` False
      minionDamageOf whiplash `shouldReturn` Just 3
      villainDamageNow `shouldReturn` 0
      getsGame (HashMap.size . gamePlayers) `shouldReturn` 1
      asked >>= \case
        Nothing -> pure ()
        Just (who, _) -> who `shouldSatisfy` (/= p)
      messages <- liftIO $ readIORef tg.ran
      messages `shouldSatisfy` elem (tshow $ EventMessage (EventId $ unCardId $ pcCardId event) ResolvedEvent)

  it "Berserker Barrage cannot attack again if its repeat damage eliminates Wolverine" do
    tg <- testGame ["wolverine", "captain-marvel"]
    inGame tg do
      let p = player1 tg
      asWolverine p
      minion <- addMinion p Minions.hydraMercenary
      setDamage p 9
      event <- payEvent p "35008" ["01088"]
      choose "defeat the minion" (isTargetChoice minion)
      choose "take two damage to repeat" (isLabel "$label.berserkerBarrageRepeat")
      getsGame (HashMap.member p . gamePlayers) `shouldReturn` False
      minionDamageOf minion `shouldReturn` Nothing
      villainDamageNow `shouldReturn` 0
      getsGame (HashMap.size . gamePlayers) `shouldReturn` 1
      asked >>= \case
        Nothing -> pure ()
        Just (who, _) -> who `shouldSatisfy` (/= p)
      messages <- liftIO $ readIORef tg.ran
      messages `shouldSatisfy` elem (tshow $ EventMessage (EventId $ unCardId $ pcCardId event) ResolvedEvent)
