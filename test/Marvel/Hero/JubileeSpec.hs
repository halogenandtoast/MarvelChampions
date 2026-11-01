module Marvel.Hero.JubileeSpec (spec) where
import TestImport
import Control.Monad (replicateM_)
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Query
import Marvel.Minion.Cards qualified as Minions

asJubilee :: IdentityId -> GameAppT ()
asJubilee p = changeForm p >> pass >> clearHand p
abilityOn :: IsRef a => a -> Natural -> Choice -> Bool
abilityOn target n = \case
  UseAbility ab -> abilitySource ab == toSource target && abilityIndex ab == n
  _ -> False
playEvent :: IdentityId -> CardCode -> [CardCode] -> GameAppT ()
playEvent p code resourceCodes = do
  card <- putInHand p code
  resources <- traverse (putInHand p) resourceCodes
  playWith card resources

spec :: Spec
spec = describe "Jubilee" do
  it "starts with nine hit points, six cards, and recovers three" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 9
      handSizeOf p `shouldReturn` 6
      setDamage p 5
      choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
      damageOf p `shouldReturn` 2

  it "Mall Rat puts Shopping Spree from deck into play and is limited per phase" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      clearHand p
      stacked <- stackDeck p ["47003"]
      choose "Mall Rat" (isNamedAbility "$label.jubileeMallRat")
      whenM (offers $ isTargetChoice $ testHead stacked) $
        choose "find Shopping Spree" (isTargetChoice $ testHead stacked)
      pass
      selectAny (SideSchemeIs Cards.jubilee47003) `shouldReturn` True
      offers (isNamedAbility "$label.jubileeMallRat") `shouldReturn` False

  it "Like totally generates a wild resource by exhausting Jubilee" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      card <- putInHand p "47007a"
      single <- putInHand p "47004"
      choose "play Firecracker" (isPlay card)
      offers (isNamedAbility "$label.jubileeLikeTotally") `shouldReturn` True
      choose "generate a wild" (isNamedAbility "$label.jubileeLikeTotally")
      whenM (offers $ isLabelStarting "$label.declareWild") $ choose "declare wild" (isLabelStarting "$label.declareWild")
      payWith [single]
      chooseVillain
      pass
      isExhaustedNow p `shouldReturn` True
      villainDamageNow `shouldReturn` 4

  for_ [("47007a","47010a"),("47007b","47010b"),("47007c","47010c")] $ \(code,energy) ->
    it (show code <> " deals four and stuns after paying two distinct types") do
      tg <- testGame ["jubilee"]
      inGame tg do
        let p = player1 tg
        asJubilee p
        playEvent p code [energy]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        selectAny (VillainEnemy <> StunnedEnemy) `shouldReturn` True

  it "Firecracker does not stun when paid with repeated energy resources" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      playEvent p "47007a" ["01088"]
      chooseVillain
      pass
      selectAny (VillainEnemy <> StunnedEnemy) `shouldReturn` False

  for_ [("47008a","47010a"),("47008b","47010b"),("47008c","47010c")] $ \(code,energy) ->
    it (show code <> " removes three and confuses after two distinct types") do
      tg <- testGame ["jubilee"]
      inGame tg do
        let p = player1 tg
        asJubilee p
        main <- mainSchemeIdNow
        resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
        before <- mainSchemeThreatNow
        playEvent p code [energy]
        chooseMainScheme
        chooseVillain
        pass
        mainSchemeThreatNow `shouldReturn` before - 3
        selectAny (VillainEnemy <> ConfusedEnemy) `shouldReturn` True

  it "Flash of Light does not confuse after repeated resources" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      playEvent p "47008a" ["01088"]
      chooseMainScheme
      pass
      selectAny (VillainEnemy <> ConfusedEnemy) `shouldReturn` False

  it "Blinding Flash stuns and confuses two different enemies" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      minion <- addMinion p Minions.hydraRegular
      playEvent p "47006" ["47010a","47007a"]
      chooseVillain
      v <- villainIdNow
      offers (isTargetChoice v) `shouldReturn` False
      choose "choose a different enemy" (isTargetChoice minion)
      pass
      selectAny (VillainEnemy <> StunnedEnemy <> ConfusedEnemy) `shouldReturn` True
      selectAny (EnemyWithId (EnemyMinionId minion) <> StunnedEnemy <> ConfusedEnemy) `shouldReturn` True

  it "Grande Finale deals an initial two plus a separate two for each paid type" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      playEvent p "47009" ["47010a","47007c"]
      replicateM_ 4 chooseVillain
      pass
      villainDamageNow `shouldReturn` 8

  it "Wolverine pierces tough and heals three after changing to alter ego" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      card <- putIntoPlay p "47002"
      let aid = allyIdOf card
      v <- villainIdNow
      resolve [VillainMessage v VillainBecomeTough]
      choose "Wolverine attacks" (abilityOn aid 301)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3
      allyDamage <$> allyIn "47002" `shouldReturn` 2
      runNow [IdentityMessage p $ ChangedToForm B]
      choose "heal Wolverine" (abilityOn aid 1)
      pass
      allyDamage <$> allyIn "47002" `shouldReturn` 0

  it "Shopping Spree blocks hero basic thwarts and its alter-ego action finds an Item" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      spree <- putIntoPlay p "47003"
      let sid = SideSchemeId $ unCardId $ pcCardId spree
      selectAny (SchemeWithId (SchemeSideSchemeId sid) <> SchemeAHeroCanBasicThwart p) `shouldReturn` False
      runNow [IdentityMessage p $ ChangedToForm B]
      pass
      item <- putInDiscard p "47004"
      choose "shop in alter ego" (abilityOn sid 1)
      pass
      sideSchemeThreat sid `shouldReturn` 1
      resolve [IdentityMessage p ReadiedIdentity]
      choose "finish shopping" (abilityOn sid 1)
      choose "find the coat" (isTargetChoice item)
      pass
      upgradeCodes `shouldReturn` ["47004"]

  it "Sunglasses adds ATK and deals one damage per resource type after an Attack event" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      glasses <- putIntoPlay p "47005"
      playEvent p "47007a" ["47010a"]
      chooseVillain
      choose "Sunglasses response" (isAbilityOfCard glasses)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8

  it "Coat adds THW and removes threat after a Thwart event using its payment" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      coat <- putIntoPlay p "47004"
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 6]
      before <- mainSchemeThreatNow
      playEvent p "47008a" ["47010a"]
      chooseMainScheme
      chooseVillain
      choose "Coat response" (isAbilityOfCard coat)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 5
