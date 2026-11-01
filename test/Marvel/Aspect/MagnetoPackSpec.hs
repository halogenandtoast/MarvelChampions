module Marvel.Aspect.MagnetoPackSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Query
import TestImport
import Marvel.Trait qualified as T
import Marvel.Modifier (Modifier(NotCountedAgainstAllyLimit))

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

playPaid :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaid p code payments = do
  card <- putInHand p code
  resources <- traverse (putInHand p) payments
  playWith card resources
  pure card

isEntryResponse :: PlayerCard -> Choice -> Bool
isEntryResponse c choice = isAbilityOfCard c choice && case choice of
  UseAbility a -> abilityIndex a == 1
  _ -> False

spec :: Spec
spec = describe "Magneto pack" do
  it "M defeats a minion with fewer remaining hit points than hers" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      enemy <- addMinion p Minions.hydraRegular
      m <- playPaid p "49012" ["01088", "01089"]
      choose "M response" (isAbilityOfCard m)
      choose "defeat minion" (isTargetChoice enemy)
      pass
      selectAny (MinionWithId enemy) `shouldReturn` False

  it "Phoenix readies and heals an X-Men ally" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      ally <- putIntoPlay p "49012"
      resolve [AllyMessage (allyIdOf ally) ExhaustedAlly, AllyMessage (allyIdOf ally) (AllyDamaged GameRef $ toDamage 2 FromAbility)]
      phoenix <- playPaid p "49014" ["01088", "01086"]
      choose "Phoenix response" (isAbilityOfCard phoenix)
      choose "ready M" (isTargetChoice $ allyIdOf ally)
      pass
      allyDamage <$> allyIn "49012" `shouldReturn` 1
      allyExhausted <$> allyIn "49012" `shouldReturn` False

  it "Cyclops increases attack damage against one chosen enemy for the phase" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      cyclops <- playPaid p "49015" ["01088", "01086"]
      choose "Cyclops response" (isAbilityOfCard cyclops)
      chooseVillain
      choose "Magneto attacks" (isHeroAttack p)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "White Queen discards a status from a character" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      resolve [IdentityMessage p IdentityConfused]
      queen <- playPaid p "49021" ["01088", "01086"]
      choose "White Queen response" (isAbilityOfCard queen)
      choose "Magneto" (isTargetChoice p)
      choose "remove confusion" (isLabel "$label.confused")
      pass
      playerIdentityConfused <$> playerAttrs p `shouldReturn` False

  it "Won't Stay Down returns an X-Men ally from discard and discards itself" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      support <- putIntoPlay p "49016"
      ally <- putInDiscard p "49012"
      choose "Won't Stay Down" (isAbilityOfCard support)
      choose "return M" (isTargetChoice ally)
      pass
      (ally `elem`) <$> handOf p `shouldReturn` True
      supportsWith "49016" `shouldReturn` []

  it "Noble Sacrifice uses printed hit points to heal and grants tough" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "49012"
      setDamage p 6
      _ <- playPaid p "49018" ["01086"]
      pass
      damageOf p `shouldReturn` 2
      playerIdentityTough <$> playerAttrs p `shouldReturn` True
      alliesWith "49012" `shouldReturn` []

  it "You Got This adds an ally's matching power and readies the hero" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "49012"
      card <- putInHand p "49019"
      resource <- putInHand p "01086"
      choose "Magneto attacks" (isHeroAttack p)
      choose "You Got This" (isPlay card)
      payWith [resource]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5
      exhaustedNow p `shouldReturn` False

  for_ [("49033", 3), ("49034", 2), ("49035", 2), ("49036", 2)] $ \(code, attack) ->
    it ("linked new recruit " <> show code <> " has its expected attack, including Surge's conditional bonus") do
      tg <- testGame ["magneto"]
      inGame tg do
        let p = player1 tg
        asHero p
        recruit <- putIntoPlay p code
        choose "recruit attacks" (\case UseAbility a -> abilitySource a == toSource (allyIdOf recruit) && abilityIndex a == 301; _ -> False)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` attack

  it "Kid Omega can spend energy to damage all enemies" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      enemy <- addMinion p Minions.hydraMercenary
      omega <- playPaid p "49013" ["01089"]
      energy <- putInHand p "01088"
      choose "Kid Omega" (isAbilityOfCard omega)
      choose "damage mode" (isLabel "$label.kidOmegaDamage")
      payWith [energy]
      pass
      villainDamageNow `shouldReturn` 1
      minionDamageOf enemy `shouldReturn` Just 1

  it "Squared Off puts the discarded minion into play before playing the discounted ally" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      ally <- putInHand p "49014"
      _ <- setEncounterDeck [Minions.hydraRegular, Minions.armoredGuard] []
      card <- putInHand p "49017"
      playWith card []
      choose "play Phoenix free" (isTargetChoice ally)
      pass
      selectAny (MinionIs Minions.hydraRegular) `shouldReturn` True
      length <$> alliesWith "49014" `shouldReturn` 1

  it "Face the Past offers only the designated nemesis, readies, draws, and bars villain attacks" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      resolve [IdentityMessage p ExhaustedIdentity]
      minions <- getNemesisMinionsOutOfPlay p
      let exodus = testHead [c | c <- minions, toCardCode c == "49028"]
      card <- playPaid p "49022" []
      offers (\choice -> any (\c -> toCardCode c /= "49028" && isTargetChoice c choice) minions) `shouldReturn` False
      choose "reveal Exodus" (isTargetChoice exodus)
      pass
      selectAny (MinionIs Minions.exodusMagneto) `shouldReturn` True
      exhaustedNow p `shouldReturn` False
      length <$> handOf p `shouldReturn` 3
      discarded <- discardOf p
      card `elem` discarded `shouldBe` False
      choose "basic attack" (isHeroAttack p)
      villain <- villainIdNow
      offers (isTargetChoice villain) `shouldReturn` False

  it "Children of the Atom grants all three X-team traits to controlled X-Men characters" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "49037"
      ally <- putIntoPlay p "49012"
      selectAny (IdentityWithId p <> IdentityWithTrait T.XForce) `shouldReturn` True
      selectAny (IdentityWithId p <> IdentityWithTrait T.XFactor) `shouldReturn` True
      selectAny (AllyWithId (allyIdOf ally) <> AllyWithTrait T.XForce) `shouldReturn` True

  it "New Recruits gives each player a distinct linked ally from the shared set-aside supply" do
    tg <- testGame ["magneto", "spider-man"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      asHero p
      clearHand q
      card <- playPaid p "49020" []
      pass
      let sid = SideSchemeId $ unCardId $ pcCardId card
      runNow [SideSchemeMessage sid $ SideSchemeThreatRemoved (IdentityRef p) 4]
      aside <- getSetAsidePlayerCards
      let surge = testHead [c | c <- aside, toCardCode c == "49033"]
          anole = testHead [c | c <- aside, toCardCode c == "49034"]
      choose "Surge" (isTargetChoice surge)
      choose "Anole" (isTargetChoice anole)
      pass
      map toCardCode <$> handOf p `shouldReturn` ["49033"]
      map toCardCode <$> handOf q `shouldReturn` ["49034"]

  it "Noble Sacrifice can discard a controlled encounter minion without looking it up as a printed ally" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      minion <- addMinion p Minions.hydraMercenary
      resolve [TakeControlOfMinion p minion]
      setDamage p 5
      _ <- playPaid p "49018" ["01086"]
      pass
      damageOf p `shouldReturn` 2
      playerIdentityTough <$> playerAttrs p `shouldReturn` True

  it "White Queen's response is absent when no character has a status card" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      queen <- playPaid p "49021" ["01088", "01086"]
      offers (isEntryResponse queen) `shouldReturn` False

  it "Phoenix's response is absent when no X-Men ally can ready or heal" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      phoenix <- playPaid p "49014" ["01088", "01086"]
      offers (isEntryResponse phoenix) `shouldReturn` False

  it "M cannot defeat a minion with exactly her remaining hit points" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- addMinion p Minions.frenzy
      m <- playPaid p "49012" ["01088", "01089"]
      offers (isEntryResponse m) `shouldReturn` False

  it "Anole's conditional bonus removes three threat with a basic thwart" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      anole <- putIntoPlay p "49034"
      choose "Anole thwarts" (\case UseAbility a -> abilitySource a == toSource (allyIdOf anole) && abilityIndex a == 300; _ -> False)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 3

  it "Bling gains toughness when entering under a Mutant's control" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "49035"
      allyTough <$> allyIn "49035" `shouldReturn` True

  it "Indra's conditional hit points let him survive three damage" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      indra <- putIntoPlay p "49036"
      resolve [AllyMessage (allyIdOf indra) $ AllyDamaged GameRef $ toDamage 3 FromAbility]
      selectAny (AllyWithId $ allyIdOf indra) `shouldReturn` True
      allyDamage <$> allyIn "49036" `shouldReturn` 3

  it "all four eligible New allies coexist with three ordinary allies without using the ally limit" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- traverse (putIntoPlay p) ["01002", "01068", "01084"]
      _ <- traverse (putIntoPlay p) ["49033", "49034", "49035", "49036"]
      length <$> selectList (AllyControlledBy $ IdentityWithId p) `shouldReturn` 7
      for_ ["01002", "01068", "01084", "49033", "49034", "49035", "49036"] $ \code ->
        length <$> alliesWith code `shouldReturn` 1

  for_ ["49033", "49034", "49035", "49036"] $ \code ->
    it ("a New ally counts against Spider-Man's ally limit when neither identity trait qualifies: " <> show code) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        recruit <- putIntoPlay p code
        modifiers <- getModifiers (AllyRef $ allyIdOf recruit)
        NotCountedAgainstAllyLimit `elem` modifiers `shouldBe` False

  it "Surge has only his printed two attack under a non-Mutant, non-X-Men hero" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      surge <- putIntoPlay p "49033"
      choose "Surge attacks" (\case UseAbility a -> abilitySource a == toSource (allyIdOf surge) && abilityIndex a == 301; _ -> False)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 2

  it "Anole has only his printed two thwart under a non-Mutant, non-X-Men hero" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      anole <- putIntoPlay p "49034"
      choose "Anole thwarts" (\case UseAbility a -> abilitySource a == toSource (allyIdOf anole) && abilityIndex a == 300; _ -> False)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 2

  it "Bling does not gain toughness under a non-Mutant, non-X-Men hero" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "49035"
      allyTough <$> allyIn "49035" `shouldReturn` False

  it "Indra has only two hit points under a non-Mutant, non-X-Men hero" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      indra <- putIntoPlay p "49036"
      resolve [AllyMessage (allyIdOf indra) $ AllyDamaged GameRef $ toDamage 2 FromAbility]
      selectAny (AllyWithId $ allyIdOf indra) `shouldReturn` False

  it "Master of Magnetism supplies exactly mental and energy resources for paid-resource effects" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      setDamage p 3
      placeMainThreat 4
      before <- mainSchemeThreatNow
      event <- putInHand p "47021"
      master <- putInHand p "49011"
      physical <- putInHand p "01069"
      playWith event [master, physical]
      chooseVillain
      chooseMainScheme
      pass
      villainDamageNow `shouldReturn` 2
      mainSchemeThreatNow `shouldReturn` before - 2
      damageOf p `shouldReturn` 1
