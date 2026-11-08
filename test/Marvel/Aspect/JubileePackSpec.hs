module Marvel.Aspect.JubileePackSpec (spec) where
import TestImport
import Control.Monad (replicateM_)
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Query
import Marvel.Message qualified as Msg
import Marvel.Cost.Types (Cost (ResourceCost))
import Marvel.Effect.Types qualified as Effect
import Marvel.Effect.Lasting (Lasting (..))
import Marvel.Modifier (Modifier (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Status ()

asJubilee :: IdentityId -> GameAppT ()
asJubilee p = changeForm p >> pass >> clearHand p
abilityOn :: IsRef a => a -> Natural -> Choice -> Bool
abilityOn target n = \case
  UseAbility ab -> abilitySource ab == toSource target && abilityIndex ab == n
  _ -> False
paying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
paying p code payments = do
  card <- putInHand p code
  resources <- traverse (putInHand p) payments
  playWith card resources
  pure card

spec :: Spec
spec = describe "Jubilee pack" do
  it "Chamber avoids attack consequential damage against a confused enemy" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      chamber <- putIntoPlay p "47011"
      v <- villainIdNow
      resolve [VillainMessage v $ VillainConfused GameRef]
      choose "Chamber attacks" (abilityOn (allyIdOf chamber) 301)
      chooseVillain
      pass
      allyDamage <$> allyIn "47011" `shouldReturn` 0
      villainDamageNow `shouldReturn` 2

  it "Chamber takes normal consequential damage against an unconfused enemy" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      chamber <- putIntoPlay p "47011"
      choose "Chamber attacks" (abilityOn (allyIdOf chamber) 301)
      chooseVillain
      pass
      allyDamage <$> allyIn "47011" `shouldReturn` 1

  it "Husk spends energy/mental to boost one attack and heal before consequential damage" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      husk <- putIntoPlay p "47012"
      let aid = allyIdOf husk
      resolve [AllyMessage aid $ AllySetDamage 1]
      resources <- putInHand p "47010a"
      choose "Husk attacks" (abilityOn aid 301)
      chooseVillain
      choose "Husk's interrupt" (abilityOn aid 1)
      payWith [resources]
      pass
      villainDamageNow `shouldReturn` 3
      allyDamage <$> allyIn "47012" `shouldReturn` 1
      resolve [AllyMessage aid ReadiedAlly]
      choose "attack without payment" (abilityOn aid 301)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5

  it "Husk's physical payment readies her after the attack" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      husk <- putIntoPlay p "47012"
      let aid = allyIdOf husk
      resources <- putInHand p "47007c"
      choose "Husk attacks" (abilityOn aid 301)
      chooseVillain
      choose "Husk's interrupt" (abilityOn aid 1)
      payWith [resources]
      pass
      allyExhausted <$> allyIn "47012" `shouldReturn` False
      villainDamageNow `shouldReturn` 2

  it "Disguise thwarts in alter ego and exhausts both upgrade and identity" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      disguise <- putIntoPlay p "47013"
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 2]
      before <- mainSchemeThreatNow
      choose "use Disguise" (isAbilityOfCard disguise)
      choose "exhaust your identity" (isTargetChoice p)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 2
      isExhaustedNow p `shouldReturn` True
      upgrades <- upgradesWith "47013"
      map upgradeExhausted upgrades `shouldBe` [True]

  it "Disguise can remove Shopping Spree's threat from alter ego" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      disguise <- putIntoPlay p "47013"
      spree <- putIntoPlay p "47003"
      item <- putInDiscard p "47005"
      let sid = SideSchemeId $ unCardId $ pcCardId spree
      choose "use Disguise" (isAbilityOfCard disguise)
      choose "exhaust your identity" (isTargetChoice p)
      choose "thwart Shopping Spree" (isTargetChoice sid)
      choose "find Sunglasses" (isTargetChoice item)
      pass
      upgradeCodes >>= (`shouldMatchList` ["47013", "47005"])

  it "Waylay deals seven after a basic thwart clears a scheme" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 1]
      card <- putInHand p "47014"
      a <- putInHand p "47010a"
      b <- putInHand p "47007c"
      choose "basic thwart" (isHeroThwart p)
      chooseMainScheme
      playWith card [a,b]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 7

  it "Waylay deals four if threat remains after the hero thwarts" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      card <- putInHand p "47014"
      a <- putInHand p "47010a"
      b <- putInHand p "47007c"
      choose "basic thwart" (isHeroThwart p)
      chooseMainScheme
      playWith card [a,b]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4

  it "Three Steps Ahead removes two sequentially for each distinct resource type" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 6]
      before <- mainSchemeThreatNow
      _ <- paying p "47015" ["47010a","47007c"]
      replicateM_ 3 chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 6

  it "Generation X gives X-Men one basic THW against itself and finds identity events" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      scheme <- putIntoPlay p "47016"
      event <- putInDiscard p "47009"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      choose "basic thwart" (isHeroThwart p)
      choose "Generation X" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 1
      resolve [IdentityMessage p ReadiedIdentity]
      choose "finish Generation X" (isHeroThwart p)
      choose "Generation X" (isTargetChoice sid)
      choose "recover Grande Finale" (isTargetChoice event)
      pass
      hand <- handOf p
      hand `shouldSatisfy` elem event
      selectAny (SideSchemeIs Cards.jubilee47016) `shouldReturn` False

  it "Synch adds one to a hero basic attack and exhausts" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      synch <- putIntoPlay p "47018"
      choose "basic attack" (isHeroAttack p)
      choose "Synch" (abilityOn (allyIdOf synch) 1)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2
      allyExhausted <$> allyIn "47018" `shouldReturn` True

  it "Synch can boost alter ego recovery and is not playable without X-Men" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      synch <- putInHand p "47018"
      _ <- putInHand p "47010a"
      _ <- putInHand p "47007c"
      offers (isPlay synch) `shouldReturn` False
      clearHand p
      synchInPlay <- putIntoPlay p "47018"
      setDamage p 5
      choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
      choose "Synch boosts REC" (abilityOn (allyIdOf synchInPlay) 1)
      pass
      damageOf p `shouldReturn` 1

  it "Cell Phone is unavailable when the only character is exhausted" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      phone <- putIntoPlay p "47019"
      runNow [IdentityMessage p Msg.ExhaustedIdentity]
      offers (isAbilityOfCard phone) `shouldReturn` False
      uses <- upgradesWith "47019"
      map upgradeUses uses `shouldBe` [3]

  it "Cell Phone is unavailable in alter-ego form without a usable ally" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      clearHand p
      phone <- putIntoPlay p "47019"
      offers (isAbilityOfCard phone) `shouldReturn` False

  it "Cell Phone respects additional basic attack costs when thwarting is forbidden" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      phone <- putIntoPlay p "47019"
      eid <- getRandom
      withGame_ $ entitiesL . effectsL . at eid ?~ Effect.Effect (Lasting Effect.EffectAttrs
        { Effect.effectId = eid
        , Effect.effectCardCode = "47019"
        , Effect.effectSource = GameRef
        , Effect.effectMatcher = IdentityEntity (IdentityWithId p)
        , Effect.effectModifiers = [CannotThwart, BasicAttackAdditionalCost (ResourceCost Nothing)]
        , Effect.effectEnds = Nothing
        })
      offers (isAbilityOfCard phone) `shouldReturn` False
      _ <- putInHand p "01088"
      offers (isAbilityOfCard phone) `shouldReturn` True

  it "Cell Phone can use a ready ally while its controller is exhausted" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      phone <- putIntoPlay p "47019"
      chamber <- putIntoPlay p "47011"
      runNow [IdentityMessage p Msg.ExhaustedIdentity]
      choose "use Cell Phone" (isAbilityOfCard phone)
      choose "choose yourself" (isTargetChoice p)
      offers (isTargetChoice p) `shouldReturn` False
      choose "choose Chamber" (isTargetChoice $ allyIdOf chamber)
      choose "Chamber attacks" (abilityOn (allyIdOf chamber) 301)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "Cell Phone offers only players with a usable basic attack or thwart" do
    tg <- testGame ["jubilee", "spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      asJubilee p
      phone <- putIntoPlay p "47019"
      choose "use Cell Phone" (isAbilityOfCard phone)
      offers (isTargetChoice other) `shouldReturn` False
      -- With only one eligible player and character, those choices resolve automatically.
      offers (isHeroAttack p) `shouldReturn` True

  it "Cell Phone spends a charge and grants one ATK for a basic attack" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      phone <- putIntoPlay p "47019"
      choose "use Cell Phone" (isAbilityOfCard phone)
      choose "choose yourself" (isTargetChoice p)
      choose "choose Jubilee" (isTargetChoice p)
      choose "make basic attack" (isHeroAttack p)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2
      uses <- upgradesWith "47019"
      map upgradeUses uses `shouldBe` [2]
      isExhaustedNow p `shouldReturn` True

  it "Multitalented resolves damage, threat removal, and healing for physical/mental/energy" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      setDamage p 3
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 2]
      before <- mainSchemeThreatNow
      _ <- paying p "47021" ["47010a","47007c"]
      chooseVillain
      chooseMainScheme
      pass
      villainDamageNow `shouldReturn` 2
      mainSchemeThreatNow `shouldReturn` before - 2
      damageOf p `shouldReturn` 1

  it "Multitalented paid only with energy heals without dealing damage or removing threat" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      setDamage p 3
      before <- mainSchemeThreatNow
      _ <- paying p "47021" ["01088","47007a"]
      pass
      damageOf p `shouldReturn` 1
      villainDamageNow `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` before

  it "Unlikely Duo confuses then deals four to a confused enemy and needs Wolverine" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      card <- putInHand p "47022"
      resources <- putInHand p "47010a"
      offers (isPlay card) `shouldReturn` False
      _ <- putIntoPlay p "47002"
      playWith card [resources]
      chooseVillain
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4
      selectAny (VillainEnemy <> ConfusedEnemy) `shouldReturn` True

  it "Mutant Mayhem returns an X-Force ally and X-Men ally then plays fresh copies" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      xforce <- putIntoPlay p "45012"
      xmen <- putIntoPlay p "47018"
      resolve [AllyMessage (allyIdOf xforce) (AllySetDamage 1), AllyMessage (allyIdOf xmen) (AllySetDamage 1)]
      _ <- paying p "47028" ["47010a","47007c"]
      pass
      allyDamage <$> allyIn "45012" `shouldReturn` 0
      allyDamage <$> allyIn "47018" `shouldReturn` 0
      length <$> alliesWith "45012" `shouldReturn` 1
      length <$> alliesWith "47018" `shouldReturn` 1

  it "Serve and Protect exhausts different X-Force/X-Men characters, prevents threat, and grants tough" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      xforce <- putIntoPlay p "45012"
      xmen <- putIntoPlay p "47018"
      card <- putInHand p "47029"
      resources <- putInHand p "47010a"
      main <- mainSchemeIdNow
      before <- mainSchemeThreatNow
      runNow (placeThreatOn (SchemeMainSchemeId main) 2)
      playWith card [resources]
      choose "exhaust the X-Force ally" (isTargetChoice $ allyIdOf xforce)
      choose "exhaust the X-Men ally" (isTargetChoice $ allyIdOf xmen)
      pass
      mainSchemeThreatNow `shouldReturn` before
      allyTough <$> allyIn "45012" `shouldReturn` True
      allyTough <$> allyIn "47018" `shouldReturn` True
      allyExhausted <$> allyIn "45012" `shouldReturn` True
      allyExhausted <$> allyIn "47018" `shouldReturn` True

  it "The Power of Justice reprint generates two wild resources for a Justice event" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 6]
      before <- mainSchemeThreatNow
      card <- putInHand p "47015"
      justice <- putInHand p "47017"
      energy <- putInHand p "47007a"
      choose "play Three Steps Ahead" (isPlay card)
      choose "double wild from Power of Justice" (isPayWith justice)
      choose "energy resource" (isPayWith energy)
      choose "declare first wild physical" \case DeclareWild 0 Physical -> True; _ -> False
      choose "declare second wild mental" \case DeclareWild 1 Mental -> True; _ -> False
      choose "finish payment" isFinishPayment
      replicateM_ 3 chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 6

  it "X-Gene reprint pays for Jubilee signature events" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      gene <- putIntoPlay p "47020"
      asJubilee p
      card <- putInHand p "47007a"
      resource <- putInHand p "47007b"
      choose "play Firecracker" (isPlay card)
      choose "X-Gene resource" (isAbilityOfCard gene)
      payWith [resource]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4

  it "Husk does not use a basic power when exhausting to defend without a DEF statistic" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      husk <- putIntoPlay p "47012"
      _ <- putInHand p "47010a"
      minion <- addMinion p Minions.hydraRegular
      runNow [MinionMessage minion $ MinionAttacks p]
      choose "Husk defends" \case AllyDefend aid _ -> aid == allyIdOf husk; _ -> False
      offers (abilityOn (allyIdOf husk) 1) `shouldReturn` False
      pass
      allyDamage <$> allyIn "47012" `shouldReturn` 2

  for_ [(StunnedStatus,301),(ConfusedStatus,300)] $ \(status,power) ->
    it (show status <> " consumes Cell Phone's bonus without carrying it into the next ally use") do
      tg <- testGame ["jubilee"]
      inGame tg do
        let p = player1 tg
        asJubilee p
        phone <- putIntoPlay p "47019"
        chamber <- putIntoPlay p "47011"
        let aid = allyIdOf chamber
        resolve [GiveStatus (AllyRef aid) status]
        choose "use Cell Phone" (isAbilityOfCard phone)
        choose "choose Chamber" (isTargetChoice aid)
        choose "use canceled power" (abilityOn aid power)
        pass
        villainDamageNow `shouldReturn` 0
        resolve [AllyMessage aid ReadiedAlly]
        choose "attack normally" (abilityOn aid 301)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
