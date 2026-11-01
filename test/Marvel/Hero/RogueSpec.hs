module Marvel.Hero.RogueSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (find)
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Query
import Marvel.Damage
import Marvel.Game.Source (getModifiers)
import Marvel.Identity (getModifiedKeywords)
import Marvel.Keyword
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Phase
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import TestImport

newtype RogueTestModifiers = RogueTestModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage RogueTestModifiers where
  runMessage msg (RogueTestModifiers attrs) = RogueTestModifiers <$> runMessage msg attrs

giveRogueTestModifiers :: EntityMatcher -> [Modifier] -> GameAppT ()
giveRogueTestModifiers matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (RogueTestModifiers EffectAttrs
    { effectId = eid, effectCardCode = "38001a", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = modifiers, effectEnds = Nothing })

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

contact :: IsCharacter a => a -> GameAppT ()
contact character = do
  choose "Skin Contact" (isNamedAbility "$label.skinContact")
  choose "Touched host" (isTargetChoice $ characterRef $ toCharacter character)
  pass

finishPlayerPhaseStart :: IdentityId -> GameAppT ()
finishPlayerPhaseStart you = do
  choose "Rogue's player-phase forced response" \case
    UseAbility ab -> abilitySource ab == toSource you && abilityIndex ab == 2
    _ -> False
  pass

hasKeyword :: IdentityId -> Keyword -> GameAppT Bool
hasKeyword p k = elem k <$> (getIdentity p >>= getModifiedKeywords)

playPaid :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaid p code resources = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resources
  playWith card payment
  pass
  pure card

allyAttrsFor :: PlayerCard -> GameAppT (Attrs Ally)
allyAttrsFor c = getsGame (toAttrs . fromMaybe (error "missing ally") . lookup (allyIdOf c) . gameAllies)

spec :: Spec
spec = describe "Rogue" do
  it "starts as Anna Marie with 11 HP, six cards and Touched set aside" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 11
      handSizeOf p `shouldReturn` 6
      (map toCardCode <$> getSetAsidePlayerCards) >>= (`shouldSatisfy` elem "38002")
      touchedHost p `shouldReturn` Nothing
      offers (isNamedAbility "$label.skinContact") `shouldReturn` False

  it "Skin Contact grants villain retaliate and is limited once per round" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      touchedHost p `shouldReturn` Just (VillainCharacter v)
      hasKeyword p (Retaliate 1) `shouldReturn` True
      offers (isNamedAbility "$label.skinContact") `shouldReturn` False
      runNow [IdentityEndedTurn p, EndRound]
      finishPlayerPhaseStart p
      touchedHost p `shouldReturn` Nothing
      offers (isNamedAbility "$label.skinContact") `shouldReturn` True

  it "Skin Contact targets another hero, granting stalwart and clearing statuses" do
    tg <- testGame ["rogue", "gambit"]
    inGame tg do
      let p = player1 tg; q = tg.seats !! 1
      asHero p
      resolve [IdentityMessage q $ ChangedToForm A]
      resolve [IdentityMessage p IdentityStunned, IdentityMessage p IdentityConfused]
      contact q
      hasKeyword p Stalwart `shouldReturn` True
      isStunned p `shouldReturn` False
      isConfused p `shouldReturn` False

  it "Skin Contact excludes Rogue and permits another alter-ego" do
    tg <- testGame ["rogue", "gambit"]
    inGame tg do
      let p = player1 tg; q = tg.seats !! 1
      asHero p
      choose "Skin Contact" (isNamedAbility "$label.skinContact")
      offers (isTargetChoice p) `shouldReturn` False
      offers (isTargetChoice q) `shouldReturn` True
      choose "the other alter-ego" (isTargetChoice q)
      pass
      hasKeyword p Stalwart `shouldReturn` False
      (member Trait.Mutant <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True

  it "Touched grants overkill on a minion and retains copied traits after moving" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.hydraMercenary
      contact m
      hasKeyword p Overkill `shouldReturn` True
      (member Trait.Hydra <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      v <- villainIdNow
      runNow [CardSpecific GameRef "findAndAttachTouched" (toJSON (p, VillainCharacter v))]
      hasKeyword p Overkill `shouldReturn` False
      hasKeyword p (Retaliate 1) `shouldReturn` True
      (member Trait.Hydra <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      runNow [EndRound]
      finishPlayerPhaseStart p
      (member Trait.Hydra <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False

  it "Touched grants Aerial without taking control of another player's ally" do
    tg <- testGame ["rogue", "gambit"]
    inGame tg do
      let p = player1 tg; q = tg.seats !! 1
      asHero p
      a <- putIntoPlay q "01084"
      contact (allyIdOf a)
      (member Trait.Aerial <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      allyController <$> allyAttrsFor a `shouldReturn` q

  it "Withdrawn sets Touched aside after changing to Anna Marie" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      runNow [IdentityEndedTurn p, EndRound]
      finishPlayerPhaseStart p
      -- The player-phase response returned Touched; attach it anew so Withdrawn
      -- must remove an attached card when Anna Marie appears.
      contact v
      changeForm p
      choose "Withdrawn forced response" (isAbilityOf p)
      pass
      touchedHost p `shouldReturn` Nothing
      hasKeyword p (Retaliate 1) `shouldReturn` False

  it "the player-phase forced response finds and sets Touched aside" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      runNow [CheckWindows [W.Window W.After $ W.PhaseBegan PlayerPhase]]
      choose "Rogue's player-phase forced response" (isAbilityOf p)
      pass
      touchedHost p `shouldReturn` Nothing

  it "Rogue's Jacket switches between friendly THW and enemy ATK bonuses" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- playPaid p "38004" ["01090"]
      getModifiers p >>= (`shouldSatisfy` notElem (AttackModifier 1))
      a <- putIntoPlay p "01084"
      contact (allyIdOf a)
      getModifiers p >>= (`shouldSatisfy` elem (ThwartModifier 1))
      v <- villainIdNow
      runNow [CardSpecific GameRef "findAndAttachTouched" (toJSON (p, VillainCharacter v))]
      getModifiers p >>= (`shouldSatisfy` elem (AttackModifier 1))
      getModifiers p >>= (`shouldSatisfy` notElem (ThwartModifier 1))

  it "Goin' Rogue removes three threat normally" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      s <- mainSchemeIdNow
      resolve [MainSchemeMessage s $ MainSchemePlaceThreat 5]
      before <- mainSchemeThreatNow
      _ <- playPaid p "38005" ["01089"]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 3

  it "Goin' Rogue removes five threat with Aerial" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "01084"
      contact (allyIdOf a)
      s <- mainSchemeIdNow
      resolve [MainSchemeMessage s $ MainSchemePlaceThreat 6]
      before <- mainSchemeThreatNow
      _ <- playPaid p "38005" ["01089"]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 5

  it "Goin' Rogue confuses an enemy with retaliate" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      s <- mainSchemeIdNow
      resolve [MainSchemeMessage s $ MainSchemePlaceThreat 4]
      _ <- playPaid p "38005" ["01089"]
      chooseMainScheme
      pass
      chooseVillain
      pass
      selectAny (ConfusedEnemy <> EnemyWithId (EnemyVillainId v)) `shouldReturn` True

  it "Southern Cross deals six damage normally" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- playPaid p "38006" ["01088", "01090"]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6

  it "Southern Cross deals eight damage with Aerial" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "01084"
      contact (allyIdOf a)
      _ <- playPaid p "38006" ["01088", "01090"]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8

  it "Southern Cross stuns the damaged enemy with retaliate" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      _ <- playPaid p "38006" ["01088", "01090"]
      chooseVillain
      pass
      selectAny (StunnedEnemy <> EnemyWithId (EnemyVillainId v)) `shouldReturn` True

  it "Energy Transfer attaches Touched, deals its cost, heals and readies Rogue" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      setDamage p 4
      resolve [IdentityMessage p ExhaustedIdentity]
      card <- putInHand p "38007"
      energy <- putInHand p "01088"
      playWith card [energy]
      chooseVillain
      pass
      damageOf p `shouldReturn` 2
      isExhaustedNow p `shouldReturn` False
      villainDamageNow `shouldReturn` 2
      v <- villainIdNow
      touchedHost p `shouldReturn` Just (VillainCharacter v)

  it "Gambit starts with three counters and his optional interrupt damages any enemy" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- playPaid p "38003" ["01088", "01090"]
      attrs <- allyAttrsFor a
      allyCounters attrs `shouldBe` 3
      choose "Gambit attacks" (isAllyAttack attrs)
      chooseVillain
      choose "Gambit's interrupt" (isAbilityOfCard a)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      allyCounters <$> allyAttrsFor a `shouldReturn` 2

  it "Superpower Adaptation cannot be played without a matching friendly discard event" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putInHand p "38009"
      offers (isPlay card) `shouldReturn` False
      v <- villainIdNow
      contact v
      offers (isPlay card) `shouldReturn` False

  it "Superpower Adaptation recovers an aspect event from its ally owner's discard" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "38012"
      contact (allyIdOf a)
      event <- putInHand p "01054"
      resolve [DiscardedCard (PlayerCard event)]
      card <- putInHand p "38009"
      playOn card event []
      pass
      (elem event <$> handOf p) `shouldReturn` True

  it "Goin' Rogue draws one card with stalwart" do
    tg <- testGame ["rogue", "gambit"]
    inGame tg do
      let p = player1 tg; q = tg.seats !! 1
      asHero p
      resolve [IdentityMessage q $ ChangedToForm A]
      contact q
      s <- mainSchemeIdNow
      resolve [MainSchemeMessage s $ MainSchemePlaceThreat 4]
      _ <- playPaid p "38005" ["01089"]
      before <- length <$> handOf p
      chooseMainScheme
      pass
      (length <$> handOf p) `shouldReturn` before + 1

  it "Southern Cross draws one card with stalwart" do
    tg <- testGame ["rogue", "gambit"]
    inGame tg do
      let p = player1 tg; q = tg.seats !! 1
      asHero p
      resolve [IdentityMessage q $ ChangedToForm A]
      contact q
      _ <- playPaid p "38006" ["01088", "01090"]
      before <- length <$> handOf p
      chooseVillain
      pass
      (length <$> handOf p) `shouldReturn` before + 1

  it "Energy Transfer pays its damage cost through tough" do
    tg <- testGame ["rogue", "gambit"]
    inGame tg do
      let p = player1 tg; q = tg.seats !! 1
      asHero p
      resolve [IdentityMessage q $ ChangedToForm A, IdentityMessage q IdentityBecomeTough]
      setDamage p 3
      card <- putInHand p "38007"
      energy <- putInHand p "01088"
      playWith card [energy]
      choose "touch the tough hero" (isTargetChoice q)
      pass
      damageOf q `shouldReturn` 0
      playerIdentityTough <$> playerAttrs q `shouldReturn` False
      damageOf p `shouldReturn` 1
      hasKeyword p Stalwart `shouldReturn` True

  it "Energy Transfer heals and readies but gains no traits when its cost defeats the host" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      setDamage p 4
      resolve [IdentityMessage p ExhaustedIdentity]
      a <- putIntoPlay p "01083"
      resolve [AllyMessage (allyIdOf a) $ AllyDamaged GameRef (toDamage 1 FromAbility)]
      card <- putInHand p "38007"
      energy <- putInHand p "01088"
      playWith card [energy]
      choose "touch Mockingbird" (isTargetChoice $ allyIdOf a)
      pass
      (member Trait.Spy <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False
      damageOf p `shouldReturn` 2
      isExhaustedNow p `shouldReturn` False
      getsGame (HashMap.member (allyIdOf a) . gameAllies) `shouldReturn` False

  it "Bulletproof Belle prevents all attack damage and gives Rogue tough" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      card <- putInHand p "38008"
      payment <- putInHand p "01090"
      runNow [VillainMessage v $ VillainAttacks p]
      playWith card [payment]
      pass
      damageOf p `shouldReturn` 0
      playerIdentityTough <$> playerAttrs p `shouldReturn` True

  it "Bulletproof Belle is unavailable when Touched is on a friendly character" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "01084"
      contact (allyIdOf a)
      card <- putInHand p "38008"
      _ <- putInHand p "01090"
      v <- villainIdNow
      runNow [VillainMessage v $ VillainAttacks p]
      offers (isPlay card) `shouldReturn` False
      pass

  it "Gambit's counters are optional and do not get spent when the interrupt is declined" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "38003"
      attrs <- allyAttrsFor a
      choose "Gambit attacks" (isAllyAttack attrs)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2
      allyCounters <$> allyAttrsFor a `shouldReturn` 3

  it "Superpower Adaptation recovers basic events through a basic ally" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "01084"
      contact (allyIdOf a)
      event <- putInHand p "01086"
      resolve [DiscardedCard $ PlayerCard event]
      card <- putInHand p "38009"
      playOn card event []
      pass
      (elem event <$> handOf p) `shouldReturn` True

  it "Superpower Adaptation takes a different hero's identity-specific event and keeps its owner" do
    tg <- testGame ["rogue", "gambit"]
    inGame tg do
      let p = player1 tg; q = tg.seats !! 1
      asHero p
      resolve [IdentityMessage q $ ChangedToForm A]
      contact q
      event <- putInHand q "37006"
      resolve [DiscardedCard $ PlayerCard event]
      card <- putInHand p "38009"
      playOn card event []
      pass
      hand <- handOf p
      let recovered = find ((== pcCardId event) . pcCardId) hand
      (pcOwner =<< recovered) `shouldBe` Just q
      (elem event <$> discardOf q) `shouldReturn` False

  it "Superpower Adaptation can recover a different aspect's event" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "38012"
      contact (allyIdOf a)
      event <- putInHand p "01060"
      resolve [DiscardedCard $ PlayerCard event]
      card <- putInHand p "38009"
      playOn card event []
      pass
      (elem event <$> handOf p) `shouldReturn` True

  it "Gambit cannot use his interrupt with no charge counters" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "38003"
      resolve [RemoveCounters (toTarget $ allyIdOf a) 3]
      attrs <- allyAttrsFor a
      choose "Gambit attacks" (isAllyAttack attrs)
      chooseVillain
      offers (isAbilityOfCard a) `shouldReturn` False
      pass
      villainDamageNow `shouldReturn` 2

  it "confusion replaces all of Goin' Rogue including its retaliate bonus" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      resolve [IdentityMessage p IdentityConfused]
      before <- mainSchemeThreatNow
      _ <- playPaid p "38005" ["01089"]
      pass
      isConfused p `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` before
      selectAny (ConfusedEnemy <> EnemyWithId (EnemyVillainId v)) `shouldReturn` False

  it "stun replaces Southern Cross and prevents its retaliate stun bonus" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      resolve [IdentityMessage p IdentityStunned]
      _ <- playPaid p "38006" ["01088", "01090"]
      pass
      isStunned p `shouldReturn` False
      villainDamageNow `shouldReturn` 0
      selectAny (StunnedEnemy <> EnemyWithId (EnemyVillainId v)) `shouldReturn` False

  it "a touched hero changing to alter-ego loses stalwart, retains friendly benefits, and updates copied traits" do
    tg <- testGame ["rogue", "gambit"]
    inGame tg do
      let p = player1 tg; q = tg.seats !! 1
      asHero p
      _ <- putIntoPlay p "38004"
      resolve [IdentityMessage q $ ChangedToForm A]
      event <- putInHand q "37006"
      resolve [DiscardedCard $ PlayerCard event]
      contact q
      card <- putInHand p "38009"
      hasKeyword p Stalwart `shouldReturn` True
      getModifiers p >>= (`shouldSatisfy` elem (ThwartModifier 1))
      (member Trait.Mutant <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False
      offers (isPlay card) `shouldReturn` True
      resolve [IdentityMessage q $ ChangedToForm B]
      hasKeyword p Stalwart `shouldReturn` False
      getModifiers p >>= (`shouldSatisfy` elem (ThwartModifier 1))
      offers (isPlay card) `shouldReturn` True
      (member Trait.Mutant <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      -- The attachment remains and its constant benefits resume in hero form.
      touchedHost p `shouldReturn` Just (IdentityCharacter q)
      resolve [IdentityMessage q $ ChangedToForm A]
      hasKeyword p Stalwart `shouldReturn` True
      getModifiers p >>= (`shouldSatisfy` elem (ThwartModifier 1))
      (member Trait.Mutant <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False

  it "copied traits end when the source leaves play and do not resume on its return" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "01083"
      contact (allyIdOf a)
      (member Trait.Spy <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      v <- villainIdNow
      runNow [CardSpecific GameRef "findAndAttachTouched" (toJSON (p, VillainCharacter v))]
      (member Trait.Spy <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      resolve [ReturnToHand $ AllyRef $ allyIdOf a]
      (member Trait.Spy <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False
      resolve [PutCardIntoPlay p a NoPayment Nothing True]
      (member Trait.Spy <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False

  for_ [False, True] $ \immune ->
    it ("Bulletproof Belle loses its granted tough to piercing, with damage immunity " <> show immune) do
      tg <- testGame ["rogue"]
      inGame tg do
        let p = player1 tg
        asHero p
        v <- villainIdNow
        contact v
        when immune $ giveRogueTestModifiers (IdentityEntity $ IdentityWithId p) [CannotTakeDamage]
        card <- putInHand p "38008"
        resource <- putInHand p "01090"
        runNow [VillainMessage v $ VillainAttacksGaining p [Piercing]]
        playWith card [resource]
        pass
        damageOf p `shouldReturn` 0
        playerIdentityTough <$> playerAttrs p `shouldReturn` False

  it "Bulletproof Belle can defend for another player against its Touched enemy" do
    tg <- testGame ["rogue", "gambit"]
    inGame tg do
      let p = player1 tg; q = tg.seats !! 1
      asHero p
      v <- villainIdNow
      contact v
      card <- putInHand p "38008"
      resource <- putInHand p "01090"
      runNow [VillainMessage v $ VillainAttacks q]
      playWith card [resource]
      pass
      damageOf p `shouldReturn` 0
      damageOf q `shouldReturn` 0
      playerIdentityTough <$> playerAttrs p `shouldReturn` True

  it "Touched follows a controlled ally restored as a minion without losing its copied traits" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.hydraMercenary
      resolve [TakeControlOfMinion p m]
      let a = AllyId $ unMinionId m
      contact a
      (member Trait.Aerial <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      hasKeyword p Overkill `shouldReturn` False
      (member Trait.Controlled <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      (member Trait.Hydra <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      resolve [RestoreControlledMinion a]
      touchedHost p `shouldReturn` Just (MinionCharacter m)
      (member Trait.Aerial <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False
      hasKeyword p Overkill `shouldReturn` True
      (member Trait.Controlled <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False
      (member Trait.Hydra <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True

  it "Goin' Rogue's confuse choice excludes confused and stalwart enemies" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      resolve [VillainMessage v $ VillainConfused GameRef]
      stalwart <- addMinion p Minions.hydraMercenary
      normal <- addMinion p Minions.hydraSoldier
      giveRogueTestModifiers (MinionEntity $ MinionWithId stalwart) [KeywordModifier Stalwart]
      s <- mainSchemeIdNow
      resolve [MainSchemeMessage s $ MainSchemePlaceThreat 4]
      _ <- playPaid p "38005" ["01089"]
      chooseMainScheme
      pass
      offers (isTargetChoice v) `shouldReturn` False
      offers (isTargetChoice stalwart) `shouldReturn` False
      choose "confuse the eligible minion" (isTargetChoice normal)
      pass
      selectAny (ConfusedEnemy <> EnemyWithId (EnemyMinionId normal)) `shouldReturn` True

  it "Gambit cannot spend a counter when every enemy is immune to damage" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "38003"
      v <- villainIdNow
      giveRogueTestModifiers (EnemyEntity $ EnemyWithId $ EnemyVillainId v) [CannotTakeDamage]
      attrs <- allyAttrsFor a
      choose "Gambit attacks" (isAllyAttack attrs)
      chooseVillain
      offers (isAbilityOfCard a) `shouldReturn` False
      pass
      allyCounters <$> allyAttrsFor a `shouldReturn` 3

  it "Gambit's damage interrupt does not offer immune enemies" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      a <- putIntoPlay p "38003"
      immune <- addMinion p Minions.weaponsRunner
      giveRogueTestModifiers (MinionEntity $ MinionWithId immune) [CannotTakeDamage]
      attrs <- allyAttrsFor a
      choose "Gambit attacks" (isAllyAttack attrs)
      chooseVillain
      choose "Gambit's interrupt" (isAbilityOfCard a)
      offers (isTargetChoice immune) `shouldReturn` False
      chooseVillain
      pass
      allyCounters <$> allyAttrsFor a `shouldReturn` 2

  it "Goin' Rogue may give a Steady enemy its second confuse status" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      contact v
      giveRogueTestModifiers (EnemyEntity $ EnemyWithId $ EnemyVillainId v) [KeywordModifier Steady]
      resolve [VillainMessage v $ VillainConfused GameRef]
      selectAny (ConfusedEnemy <> EnemyWithId (EnemyVillainId v)) `shouldReturn` False
      s <- mainSchemeIdNow
      resolve [MainSchemeMessage s $ MainSchemePlaceThreat 4]
      _ <- playPaid p "38005" ["01089"]
      chooseMainScheme
      pass
      chooseVillain
      pass
      selectAny (ConfusedEnemy <> EnemyWithId (EnemyVillainId v)) `shouldReturn` True

  it "copied minion traits end on defeat and never resume when the same physical minion is revealed again" do
    tg <- testGame ["rogue"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- encounterCard Minions.hydraMercenary
      resolve [RevealEncounterCard p card]
      let m = minionOf card
      contact m
      (member Trait.Hydra <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` True
      resolve [MinionMessage m $ MinionDamaged GameRef $ toDamage 10 FromAbility]
      minionDamageOf m `shouldReturn` Nothing
      (member Trait.Hydra <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False
      resolve [RevealEncounterCard p card]
      minionDamageOf m `shouldReturn` Just 0
      (member Trait.Hydra <$> getCharacterTraits (IdentityCharacter p)) `shouldReturn` False
