module Marvel.Aspect.IcemanPackSpec (spec) where

import Marvel.Ability qualified as Ability
import Marvel.Iceman.Helpers qualified as Ice
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import TestImport

asIceman :: IdentityId -> GameAppT ()
asIceman p = changeForm p >> settle >> clearHand p

settle :: GameAppT ()
settle = do
  pass
  cs <- asked
  case cs >>= headMay . filter forced . snd of
    Just c -> choose "mandatory ability" (== c) >> settle
    Nothing -> pure ()
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse, Ability.WhenRevealed]
    _ -> False

paying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
paying p code resources = do
  card <- putInHand p code
  payments <- traverse (putInHand p) resources
  playWith card payments
  pure card

spec :: Spec
spec = describe "Iceman pack" do
  it "Shark-Girl gains one ATK per player upgrade on her attacked enemy" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      shark <- putIntoPlay p "46012"
      v <- villainIdNow
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      pass
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      pass
      choose "Shark-Girl attacks" \case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf shark) && abilityIndex ab == 301; _ -> False
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4

  it "Shark-Girl does not gain an attack bonus against a bare enemy" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      shark <- putIntoPlay p "46012"
      choose "Shark-Girl attacks" \case UseAbility ab -> abilitySource ab == AllyRef (allyIdOf shark) && abilityIndex ab == 301; _ -> False
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  it "Glob's enter-play response damages an upgraded enemy without being an attack" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      v <- villainIdNow
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      pass
      glob <- paying p "46013" ["01088","01089"]
      choose "Glob response" (isAbilityOfCard glob)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  it "Glob requires the current identity to have X-Men" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      glob <- putInHand p "46013"
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      offers (isPlay glob) `shouldReturn` False

  it "Take That requires an upgraded enemy and deals seven damage" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      card <- putInHand p "46016"
      payment <- traverse (putInHand p) ["01088","01089"]
      offers (isPlay card) `shouldReturn` False
      v <- villainIdNow
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      pass
      playWith card payment
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 7

  it "Suppressing Fire heals its controller after the hero defeats the attached minion" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      mid <- addMinion p Minions.hydraRegular
      fire <- putInHand p "46014"
      playOn fire mid []
      setDamage p 4
      choose "attack minion" (isHeroAttack p)
      choose "minion target" (isTargetChoice mid)
      choose "decline Freeze" isNoResponse
      choose "heal with suppressing fire" (isAbilityOfCard fire)
      settle
      damageOf p `shouldReturn` 2

  it "Surprise Move can follow Freeze in the same attack window and readies on defeat" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      mid <- addMinion p Minions.vulture
      move <- putInHand p "46015"
      payment <- putInHand p "01093"
      choose "basic attack" (isHeroAttack p)
      choose "minion" (isTargetChoice mid)
      offers (isPlay move) `shouldReturn` False
      choose "Freeze" (isNamedAbility "$label.freeze")
      playWith move [payment]
      settle
      isExhaustedNow p `shouldReturn` False
      selectList (MinionWithId mid) `shouldReturn` []

  it "Beak removes one threat per controlled X-Men ally, counting himself" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      _ <- putIntoPlay p "46012"
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main (MainSchemePlaceThreat 5)]
      beak <- paying p "46020" ["01088"]
      choose "Beak response" (isAbilityOfCard beak)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 3

  it "Shadowcat requires X-Men and removes every printed side-scheme icon for the round" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      shadowcat <- putInHand p "46019"
      _ <- traverse (putInHand p) ["01088","01089"]
      offers (isPlay shadowcat) `shouldReturn` False
      asIceman p
      scheme <- sideSchemeOf <$> revealNow p Schemes.breakinAndTakin
      settle
      shadowcat' <- paying p "46019" ["01088","01089"]
      choose "Shadowcat response" (isAbilityOfCard shadowcat')
      choose "side scheme" (isTargetChoice scheme)
      pass
      icons <- getModifiers (SideSchemeRef scheme)
      SuppressSchemeIcons `elem` icons `shouldBe` True

  it "Keep Up the Pressure searches an Attack event and adds one damage during this phase" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      pressure <- paying p "46018" []
      attack <- putInDiscard p "46009"
      unrelated <- putInDiscard p "46011"
      let sid = SideSchemeId $ unCardId $ pcCardId pressure
      runNow [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p) 2)]
      offers (isTargetChoice unrelated) `shouldReturn` False
      choose "find Attack event" (isTargetChoice attack)
      pass
      hand <- handOf p
      attack `elem` hand `shouldBe` True
      payments <- traverse (putInHand p) ["01088"]
      playWith attack payments
      choose "first option" (isLabel "$label.arcticAttackFreeze")
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5

  it "Take That cannot attack an upgraded villain while a Guard minion blocks it" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      villain <- villainIdNow
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId villain) >>= runNow
      pass
      _ <- addMinion p Minions.hydraMercenary
      card <- putInHand p "46016"
      _ <- traverse (putInHand p) ["01088", "01086"]
      offers (isPlay card) `shouldReturn` False
