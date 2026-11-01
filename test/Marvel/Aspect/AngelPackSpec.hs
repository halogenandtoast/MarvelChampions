module Marvel.Aspect.AngelPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Modifier
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait (Trait (XForce))
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Window qualified as W
import Marvel.Villain.Types qualified as Villain
import TestImport

asAngel :: IdentityId -> GameAppT ()
asAngel p = changeFormTo p A >> pass >> clearHand p

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

paying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
paying p card codes = traverse (putInHand p) codes >>= playWith card

allyDamageNow :: CardCode -> GameAppT Natural
allyDamageNow code = allyDamage <$> allyIn code

newtype AngelPackModifiers = AngelPackModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage AngelPackModifiers where
  runMessage msg (AngelPackModifiers attrs) = AngelPackModifiers <$> runMessage msg attrs

preventPlayerHealing :: IdentityId -> GameAppT ()
preventPlayerHealing who = modifiersOn (IdentityEntity $ IdentityWithId who) [CannotBeHealedByPlayerCards]

modifiersOn :: EntityMatcher -> [Modifier] -> GameAppT ()
modifiersOn matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (AngelPackModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = matcher
    , effectModifiers = modifiers, effectEnds = Nothing })

-- An attack can first offer events from the hand before asking for defenders.
declineUntilDefenders :: GameAppT ()
declineUntilDefenders = do
  ready <- offers $ \case
    Defend _ -> True
    AllyDefend _ _ -> True
    _ -> False
  unless ready do
    decline <- offers isNoResponse
    when decline $ choose "decline pre-defense interrupt" isNoResponse >> declineUntilDefenders

spec :: Spec
spec = describe "Angel pack" do
  it "Elixir heals another friendly character after attacking" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      setDamage p 3
      elixir <- putIntoPlay p "42011"
      choose "Elixir attacks" (cardAbility elixir 301)
      chooseVillain
      choose "Elixir heals" (cardAbility elixir 1)
      choose "heal Angel" (isTargetChoice p)
      pass
      damageOf p `shouldReturn` 2
      allyDamageNow "42011" `shouldReturn` 1

  it "Elixir also heals after thwarting and cannot heal himself" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      setDamage p 2
      placeMainThreat 3
      elixir <- putIntoPlay p "42011"
      choose "Elixir thwarts" (cardAbility elixir 300)
      chooseMainScheme
      choose "Elixir response" (cardAbility elixir 1)
      offers (isTargetChoice $ allyIdOf elixir) `shouldReturn` False
      choose "heal Angel" (isTargetChoice p)
      pass
      damageOf p `shouldReturn` 1

  it "Elixir needs an X-Force or X-Men identity to be played" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      clearHand p
      elixir <- putInHand p "42011"
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      offers (isPlay elixir) `shouldReturn` False
      changeFormTo p A
      pass
      offers (isPlay elixir) `shouldReturn` True

  it "Siryn stuns a minion after attacking" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      siryn <- putIntoPlay p "42012"
      mid <- addMinion p Minions.hydraSoldier
      choose "Siryn attacks" (cardAbility siryn 301)
      choose "attack minion" (isTargetChoice mid)
      choose "Siryn response" (cardAbility siryn 1)
      choose "stun minion" (isTargetChoice mid)
      pass
      selectAny (MinionWithId mid <> StunnedMinion) `shouldReturn` True

  it "Siryn has no stun response without a minion" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      siryn <- putIntoPlay p "42012"
      choose "Siryn attacks" (cardAbility siryn 301)
      chooseVillain
      offers (cardAbility siryn 1) `shouldReturn` False

  it "Cannonball optionally reduces consequential damage by current Aerial cards in hand" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      cannonball <- putIntoPlay p "42020"
      _ <- putInHand p "42006"
      _ <- putInHand p "42007"
      choose "Cannonball attacks" (cardAbility cannonball 301)
      chooseVillain
      choose "Cannonball interrupt" (cardAbility cannonball 1)
      pass
      allyDamageNow "42020" `shouldReturn` 0
      villainDamageNow `shouldReturn` 2

  it "Cannonball cannot reduce consequential damage with no Aerial cards" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      cannonball <- putIntoPlay p "42020"
      choose "Cannonball attacks" (cardAbility cannonball 301)
      chooseVillain
      offers (cardAbility cannonball 1) `shouldReturn` False
      pass
      alliesWith "42020" `shouldReturn` []

  it "Cannonball can decline his consequential-damage interrupt" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      cannonball <- putIntoPlay p "42020"
      _ <- putInHand p "42006"
      _ <- putInHand p "42007"
      choose "Cannonball attacks" (cardAbility cannonball 301)
      chooseVillain
      offers (cardAbility cannonball 1) `shouldReturn` True
      pass
      alliesWith "42020" `shouldReturn` []

  it "Warpath enters tough and plays a paid Hero Action event after defending" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      warpath <- putIntoPlay p "42013"
      allyTough <$> allyIn "42013" `shouldReturn` True
      flight <- putInHand p "42006"
      payment <- putInHand p "01088"
      placeMainThreat 6
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      declineUntilDefenders
      choose "Warpath defends" (\case AllyDefend aid _ -> aid == allyIdOf warpath; _ -> False)
      choose "Warpath response" (cardAbility warpath 1)
      choose "Natural Flight" (isTargetChoice flight)
      payWith [payment]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 4
      allyTough <$> allyIn "42013" `shouldReturn` False
      discardOf p >>= (`shouldSatisfy` elem payment)

  it "Warpath does not offer Hero Action play with only an interrupt event" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      warpath <- putIntoPlay p "42013"
      _ <- putInHand p "42004"
      _ <- putInHand p "01088"
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      declineUntilDefenders
      choose "Warpath defends" (\case AllyDefend aid _ -> aid == allyIdOf warpath; _ -> False)
      offers (cardAbility warpath 1) `shouldReturn` False

  it "Aerial Intervention exhausts an Aerial character and prevents three attack damage" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      intervention <- putInHand p "42014"
      start [CheckWindows [W.Window W.When $ W.IdentityTakeDamage p (toDamage 5 FromAttack)], IdentityMessage p $ IdentityDamaged GameRef (toDamage 5 FromAttack)]
      playWith intervention []
      choose "exhaust Angel" (isTargetChoice p)
      pass
      damageOf p `shouldReturn` 2
      exhaustedNow p `shouldReturn` True

  it "Aerial Intervention does not prevent damage from an ability" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      intervention <- putInHand p "42014"
      start [CheckWindows [W.Window W.When $ W.IdentityTakeDamage p (toDamage 2 FromAbility)], IdentityMessage p $ IdentityDamaged GameRef (toDamage 2 FromAbility)]
      offers (isPlay intervention) `shouldReturn` False
      pass
      damageOf p `shouldReturn` 2

  it "Taunt allows Angel to defend and draws three after the attack" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      taunt <- putInHand p "42016"
      payment <- putInHand p "01091"
      _ <- putIntoPlay p "42013"
      playWith taunt [payment]
      offers isDefend `shouldReturn` True
      offers (\case AllyDefend _ _ -> True; _ -> False) `shouldReturn` False
      choose "Angel defends" isDefend
      pass
      length <$> handOf p `shouldReturn` 3

  it "Flying Formation readies up to three distinct Aerial characters" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      cannonball <- putIntoPlay p "42020"
      siryn <- putIntoPlay p "42012"
      resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf cannonball) ExhaustedAlly, AllyMessage (allyIdOf siryn) ExhaustedAlly]
      formation <- putInHand p "42031"
      paying p formation ["01088", "01089"]
      choose "ready Angel" (isTargetChoice p)
      choose "ready Cannonball" (isTargetChoice $ allyIdOf cannonball)
      choose "ready Siryn" (isTargetChoice $ allyIdOf siryn)
      pass
      exhaustedNow p `shouldReturn` False
      allyExhausted <$> allyIn "42020" `shouldReturn` False
      allyExhausted <$> allyIn "42012" `shouldReturn` False

  it "Flying Formation lets its controller stop after one character" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      cannonball <- putIntoPlay p "42020"
      resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyIdOf cannonball) ExhaustedAlly]
      formation <- putInHand p "42031"
      paying p formation ["01088", "01089"]
      choose "ready Angel" (isTargetChoice p)
      choose "stop" (isLabel "$label.done")
      pass
      allyExhausted <$> allyIn "42020" `shouldReturn` True

  it "Bombs Away damages the villain and only the selected player's minions" do
    tg <- testGame ["angel", "spider-man"]
    inGame tg do
      let p = player1 tg
      asAngel p
      let other = player2 tg
      mine <- addMinion p Minions.hydraSoldier
      theirs <- addMinion other Minions.hydraSoldier
      bomb <- putInHand p "42029"
      payment <- putInHand p "01088"
      playOn bomb other [payment]
      choose "exhaust Angel" (isTargetChoice p)
      pass
      villainDamageNow `shouldReturn` 3
      minionDamageOf mine `shouldReturn` Just 0
      minionDamageOf theirs `shouldReturn` Just 3

  it "Soaring Acrobatics adds one to an Aerial hero's basic attack once exhausted" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      acrobatics <- putIntoPlay p "42023"
      choose "Angel attacks" (isHeroAttack p)
      choose "Acrobatics" (cardAbility acrobatics 1)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2
      upgradeIsExhausted (upgradeIdOf acrobatics) `shouldReturn` True

  it "Soaring Acrobatics adds one to an Aerial ally's basic attack" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      acrobatics <- putIntoPlay p "42023"
      siryn <- putIntoPlay p "42012"
      choose "Siryn attacks" (cardAbility siryn 301)
      chooseVillain
      choose "Acrobatics" (cardAbility acrobatics 1)
      pass
      villainDamageNow `shouldReturn` 3

  it "Soaring Acrobatics does not answer a non-Aerial ally's basic power" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      acrobatics <- putIntoPlay p "42023"
      elixir <- putIntoPlay p "42011"
      choose "Elixir attacks" (cardAbility elixir 301)
      chooseVillain
      offers (cardAbility acrobatics 1) `shouldReturn` False

  it "X-Force Recruit attaches to an ally, granting a hit point and X-Force" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      nick <- putIntoPlay p "01084"
      recruit <- putInHand p "42032"
      playOn recruit (allyIdOf nick) []
      attrs <- allyIn "01084"
      Ally.getModifiedHitPoints attrs `shouldReturn` 4
      selectAny (AllyWithId (allyIdOf nick) <> AllyWithTrait XForce) `shouldReturn` True
      another <- putInHand p "42032"
      choose "second recruit" (isPlay another)
      offers (isTargetChoice $ allyIdOf nick) `shouldReturn` False

  it "X-Force Recruit is not playable by an identity without X-Force" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      recruit <- putInHand p "42032"
      offers (isPlay recruit) `shouldReturn` False

  it "Eyes in the Sky cancels a non-Elite minion and reveals a replacement" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      eyes <- putIntoPlay p "42030"
      _ <- stackEncounterDeck [Treacheries.advance]
      _ <- revealNow p Minions.hydraSoldier
      choose "Eyes in the Sky" (cardAbility eyes 1)
      choose "exhaust Angel" (isTargetChoice p)
      pass
      selectAny (MinionIs Minions.hydraSoldier) `shouldReturn` False
      encounterDiscardCodes >>= (`shouldSatisfy` elem "01182")
      upgradeCodes >>= (`shouldSatisfy` notElem "42030")

  it "Angel's Aerie gains fatigue after defending and spends all to heal in alter ego" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      aerie <- putIntoPlay p "42018"
      setDamage p 4
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      choose "defend" isDefend
      choose "fatigue" (cardAbility aerie 1)
      pass
      getCountersOn (supportOf aerie) `shouldReturn` 1
      resolve [IdentityMessage p (ChangedToForm B)]
      before <- damageOf p
      choose "heal with Aerie" (cardAbility aerie 2)
      damageOf p `shouldReturn` before - 1
      getCountersOn (supportOf aerie) `shouldReturn` 0

  it "Containment Strategy removes two threat after an undamaged defense" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      sid <- sideSchemeOf <$> revealNow p Schemes.crowdControl
      pass
      strategy <- putInHand p "42019"
      payment <- putInHand p "01091"
      playOn strategy sid [payment]
      before <- sideSchemeThreat sid
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      choose "defend" isDefend
      choose "Containment Strategy" (cardAbility strategy 1)
      pass
      sideSchemeThreat sid `shouldReturn` before - 2

  it "Render Medical Aid distributes five healing among controlled characters" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      setDamage p 6
      aid <- putInHand p "42017"
      playWith aid []
      let sid = SideSchemeId $ unCardId $ pcCardId aid
      start [SideSchemeMessage sid $ SideSchemeThwarted (toSource p) 3]
      pressWhenRevealed
      for_ [1..5 :: Int] $ \_ -> choose "heal Angel" (isTargetChoice p)
      pass
      damageOf p `shouldReturn` 1
      map toCardCode <$> getPlayerVictoryDisplay `shouldReturn` ["42017"]

  it "Eyes in the Sky cannot cancel an Elite minion" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      eyes <- putIntoPlay p "42030"
      _ <- revealNow p Minions.modok
      offers (cardAbility eyes 1) `shouldReturn` False
      pass
      selectAny (MinionIs Minions.modok) `shouldReturn` True

  it "Soaring Acrobatics improves an Aerial ally's basic thwart" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      placeMainThreat 6
      before <- mainSchemeThreatNow
      acrobatics <- putIntoPlay p "42023"
      siryn <- putIntoPlay p "42012"
      choose "Siryn thwarts" (cardAbility siryn 300)
      chooseMainScheme
      choose "Acrobatics" (cardAbility acrobatics 1)
      pass
      mainSchemeThreatNow `shouldReturn` before - 3

  it "Soaring Acrobatics does not trigger when a stun replaces a basic attack" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      acrobatics <- putIntoPlay p "42023"
      resolve [GiveStatus (IdentityRef p) StunnedStatus]
      choose "remove stun" (isHeroAttack p)
      offers (cardAbility acrobatics 1) `shouldReturn` False
      upgradeIsExhausted (upgradeIdOf acrobatics) `shouldReturn` False
      villainDamageNow `shouldReturn` 0

  it "Aerial Intervention also protects a friendly ally from attack damage" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      elixir <- putIntoPlay p "42011"
      intervention <- putInHand p "42014"
      let aid = allyIdOf elixir
      start [CheckWindows [W.Window W.When $ W.AllyTakeDamage aid (toDamage 2 FromAttack)], AllyMessage aid $ AllyDamaged GameRef (toDamage 2 FromAttack)]
      playWith intervention []
      choose "exhaust Angel" (isTargetChoice p)
      pass
      allyDamageNow "42011" `shouldReturn` 0

  it "Containment Strategy removes only one when the defending hero takes damage" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      sid <- sideSchemeOf <$> revealNow p Schemes.crowdControl
      pass
      strategy <- putInHand p "42019"
      payment <- putInHand p "01091"
      playOn strategy sid [payment]
      before <- sideSchemeThreat sid
      vid <- villainIdNow
      modifiersOn (EnemyEntity $ EnemyWithId $ EnemyVillainId vid) [AttackModifier 1]
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      choose "defend" isDefend
      damageOf p `shouldReturn` 1
      choose "Containment Strategy" (cardAbility strategy 1)
      pass
      sideSchemeThreat sid `shouldReturn` before - 1

  it "Taunt still draws three when stun prevents the attack and clears its restriction" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      warpath <- putIntoPlay p "42013"
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned GameRef)]
      taunt <- putInHand p "42016"
      paying p taunt ["01091"]
      pass
      length <$> handOf p `shouldReturn` 3
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      declineUntilDefenders
      offers (\case AllyDefend aid _ -> aid == allyIdOf warpath; _ -> False) `shouldReturn` True

  it "Angel's Aerie has no heal action without fatigue counters" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      setDamage p 3
      aerie <- putIntoPlay p "42018"
      offers (cardAbility aerie 2) `shouldReturn` False


  it "Aerial Intervention can prevent a hero's attack damage to the villain" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      siryn <- putIntoPlay p "42012"
      intervention <- putInHand p "42014"
      choose "Angel attacks" (isHeroAttack p)
      chooseVillain
      playWith intervention []
      choose "exhaust Siryn" (isTargetChoice $ allyIdOf siryn)
      pass
      villainDamageNow `shouldReturn` 0
      allyExhausted <$> allyIn "42012" `shouldReturn` True

  it "Aerial Intervention can prevent an ally's attack damage to a minion" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      siryn <- putIntoPlay p "42012"
      mid <- addMinion p Minions.hydraSoldier
      intervention <- putInHand p "42014"
      choose "Siryn attacks" (cardAbility siryn 301)
      choose "target minion" (isTargetChoice mid)
      playWith intervention []
      choose "exhaust Angel" (isTargetChoice p)
      pass
      minionDamageOf mid `shouldReturn` Just 0
      exhaustedNow p `shouldReturn` True


  it "Elixir does not offer healing when the only damaged other character cannot be healed" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      setDamage p 3
      preventPlayerHealing p
      elixir <- putIntoPlay p "42011"
      choose "Elixir attacks" (cardAbility elixir 301)
      chooseVillain
      offers (cardAbility elixir 1) `shouldReturn` False
      pass
      damageOf p `shouldReturn` 3

  it "Angel's Aerie does not spend fatigue if the damaged identity cannot be healed" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      aerie <- putIntoPlay p "42018"
      setDamage p 4
      vid <- villainIdNow
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      choose "defend" isDefend
      choose "fatigue" (cardAbility aerie 1)
      pass
      resolve [IdentityMessage p (ChangedToForm B)]
      preventPlayerHealing p
      refreshTurn
      offers (cardAbility aerie 2) `shouldReturn` False
      getCountersOn (supportOf aerie) `shouldReturn` 1

  it "Render Medical Aid excludes unhealable characters from mandatory healing allocations" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      setDamage p 5
      preventPlayerHealing p
      warpath <- putIntoPlay p "42013"
      resolve [AllyMessage (allyIdOf warpath) (AllySetDamage 3)]
      aid <- putInHand p "42017"
      playWith aid []
      let sid = SideSchemeId $ unCardId $ pcCardId aid
      start [SideSchemeMessage sid $ SideSchemeThwarted (toSource p) 3]
      pressWhenRevealed
      offers (isTargetChoice p) `shouldReturn` False
      for_ [1..3 :: Int] $ \_ -> choose "heal Warpath" (isTargetChoice $ allyIdOf warpath)
      pass
      allyDamageNow "42013" `shouldReturn` 0
      damageOf p `shouldReturn` 5


  it "Aerial Intervention prevents the effective damage after an enemy's attack-damage cap" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      vid <- villainIdNow
      modifiersOn (EnemyEntity $ EnemyWithId $ EnemyVillainId vid) [AttackDamageCap 2]
      intervention <- putInHand p "42014"
      dive <- putInHand p "42007"
      paying p dive ["01088", "01091"]
      chooseVillain
      playWith intervention []
      choose "exhaust Angel" (isTargetChoice p)
      pass
      villainDamageNow `shouldReturn` 0
      exhaustedNow p `shouldReturn` True

  it "Aerial Intervention prevents three damage after an enemy's attack-damage reduction" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      vid <- villainIdNow
      modifiersOn (EnemyEntity $ EnemyWithId $ EnemyVillainId vid) [AttackDamageReduction 2]
      intervention <- putInHand p "42014"
      dive <- putInHand p "42007"
      paying p dive ["01088", "01091"]
      chooseVillain
      playWith intervention []
      choose "exhaust Angel" (isTargetChoice p)
      pass
      villainDamageNow `shouldReturn` 1

  it "Aerial Intervention prevents three damage after a minion's damage multiplier" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      mid <- addMinion p Minions.hydraSoldier
      modifiersOn (EnemyEntity $ EnemyWithId $ EnemyMinionId mid)
        [DamageMultiplierFromResource Physical 2, HitPointModifier 20]
      intervention <- putInHand p "42014"
      dive <- putInHand p "42007"
      paying p dive ["01088", "01091"]
      choose "dive at minion" (isTargetChoice mid)
      playWith intervention []
      choose "exhaust Angel" (isTargetChoice p)
      pass
      minionDamageOf mid `shouldReturn` Just 9

  it "Soaring Acrobatics adds one to an Aerial hero's basic defense for that attack" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      acrobatics <- putIntoPlay p "42023"
      vid <- villainIdNow
      modifiersOn (EnemyEntity $ EnemyWithId $ EnemyVillainId vid) [AttackModifier 1]
      start [VillainMessage vid (VillainAttacksWithoutBoost p)]
      choose "Angel defends" isDefend
      choose "Acrobatics adds defense" (cardAbility acrobatics 1)
      pass
      damageOf p `shouldReturn` 0
      upgradeIsExhausted (upgradeIdOf acrobatics) `shouldReturn` True


  it "Aerial Intervention preventing all piercing damage still discards the enemy's tough status" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      changeFormTo p C
      pass
      clearHand p
      vid <- villainIdNow
      modifiersOn (EnemyEntity $ EnemyWithId $ EnemyVillainId vid) [AttackDamageCap 2]
      resolve [GiveStatus (VillainRef vid) ToughStatus]
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True
      intervention <- putInHand p "42014"
      dive <- putInHand p "42007"
      paying p dive ["01088", "01091"]
      chooseVillain
      playWith intervention []
      choose "exhaust Archangel" (isTargetChoice p)
      pass
      villainDamageNow `shouldReturn` 0
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` False


  it "Aerial Intervention preventing all piercing attack damage still discards the hero's tough status" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      intervention <- putInHand p "42014"
      let damage = withPiercing $ toDamage 2 FromAttack
      start [CheckWindows [W.Window W.When $ W.IdentityTakeDamage p damage], IdentityMessage p $ IdentityDamaged GameRef damage]
      playWith intervention []
      choose "exhaust Angel" (isTargetChoice p)
      pass
      damageOf p `shouldReturn` 0
      playerIdentityTough <$> playerAttrs p `shouldReturn` False

  it "Aerial Intervention preventing all piercing attack damage still discards an ally's tough status" do
    tg <- testGame ["angel"]
    inGame tg do
      let p = player1 tg
      asAngel p
      elixir <- putIntoPlay p "42011"
      let aid = allyIdOf elixir
      resolve [GiveStatus (AllyRef aid) ToughStatus]
      intervention <- putInHand p "42014"
      let damage = withPiercing $ toDamage 2 FromAttack
      start [CheckWindows [W.Window W.When $ W.AllyTakeDamage aid damage], AllyMessage aid $ AllyDamaged GameRef damage]
      playWith intervention []
      choose "exhaust Angel" (isTargetChoice p)
      pass
      allyDamageNow "42011" `shouldReturn` 0
      allyTough <$> allyIn "42011" `shouldReturn` False
