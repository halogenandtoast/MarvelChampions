module Marvel.Hero.WaspSpec (spec) where

import Marvel.Ally.Types qualified as Ally
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Matchers (AllyMatcher (AllyWithTrait), IdentityMatcher (IdentityWithTrait), allyMatches, identityMatches)
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Trait (Trait (Aerial, Giant, Tiny))
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types (villainIsTough)
import TestImport

wasp :: IO TestGame
wasp = testGame ["wasp"]

-- | Wasp in the form given (A Tiny, B Nadia Van Dyne, C Giant), her hand empty.
inForm :: IdentityId -> Side -> GameAppT ()
inForm p side = do
  clearHand p
  changeFormTo p side
  pass

data Stats = Stats {thwart :: Natural, attack :: Natural}

-- | The hero's THW and ATK as modified.
heroStats :: IdentityId -> GameAppT Stats
heroStats p =
  currentIdentity <$> getPlayer p >>= \case
    HeroSide h -> Stats <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h)
    AlterEgoSide _ -> pure (Stats 0 0)

-- | Shocker, with 1 hit point left.
weakShocker :: IdentityId -> GameAppT MinionId
weakShocker p = do
  m <- addMinion p Minions.shocker
  resolve [MinionMessage m (MinionDamaged GameRef (toDamage 2 FromAbility))]
  pure m

rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Treacheries.imTough]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . (.playerIdentityDeck) <$> playerAttrs p

isSmallButMighty :: Choice -> Bool
isSmallButMighty = isNamedAbility "Small but Mighty"

spec :: Spec
spec = describe "Wasp" do
  describe "Small but Mighty (Tiny)" do
    it "her basic attack defeats a minion: 1 damage to the villain" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        m <- weakShocker p
        choose "attack" (isHeroAttack p)
        choose "Shocker" (isTargetChoice m)
        minionDamageOf m `shouldReturn` Nothing
        choose "Small but Mighty" isSmallButMighty
        villainDamageNow `shouldReturn` 1

    it "her basic thwart defeats a side scheme: 1 damage to the villain" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        sid <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        before <- sideSchemeThreat sid
        resolve [SideSchemeMessage sid (SideSchemeThwarted GameRef (before - 1))]
        sideSchemeThreat sid `shouldReturn` 1
        choose "thwart" (isHeroThwart p)
        choose "Bomb Scare" (isTargetChoice (SchemeSideSchemeId sid))
        choose "Small but Mighty" isSmallButMighty
        villainDamageNow `shouldReturn` 1

    it "an event she plays defeats a minion: 1 damage to the villain" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        m <- addMinion p Minions.shocker
        assault <- putInHand p "01053"
        genius <- putInHand p "01089"
        playWith assault [genius]
        choose "Shocker" (isTargetChoice m)
        minionDamageOf m `shouldReturn` Nothing
        choose "Small but Mighty" isSmallButMighty
        villainDamageNow `shouldReturn` 1

    it "is still offered after another response to the same defeat" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        _ <- putIntoPlay p "01063"
        placeMainThreat 3
        threat <- mainSchemeThreatNow
        m <- weakShocker p
        choose "attack" (isHeroAttack p)
        choose "Shocker" (isTargetChoice m)
        [room] <- supportsWith "01063"
        offers isSmallButMighty `shouldReturn` True
        choose "use Interrogation Room" (isAbilityOf (supportId room))
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (threat - 1)
        choose "Small but Mighty" isSmallButMighty
        villainDamageNow `shouldReturn` 1

    it "answers a side scheme removed from the game as it is defeated" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        _ <- putInHand p "13002"
        card <- encounterCard SideSchemes.hydraPrison
        -- Ant-Man goes beneath it, so it has his cost in threat
        resolve [RevealEncounterCard p card]
        let sid = sideSchemeOf card
        before <- sideSchemeThreat sid
        resolve [SideSchemeMessage sid (SideSchemeThwarted GameRef (before - 1))]
        sideSchemeThreat sid `shouldReturn` 1
        choose "thwart" (isHeroThwart p)
        choose "Hydra Prison" (isTargetChoice (SchemeSideSchemeId sid))
        choose "Small but Mighty" isSmallButMighty
        villainDamageNow `shouldReturn` 1
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "04122")

    it "an ally's defeat of a minion is not Wasp's" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        _ <- putIntoPlay p "13002"
        m <- weakShocker p
        antMan <- allyIn "13002"
        choose "Ant-Man attacks" (isAllyAttack antMan)
        choose "Shocker" (isTargetChoice m)
        minionDamageOf m `shouldReturn` Nothing
        offers isSmallButMighty `shouldReturn` False
        villainDamageNow `shouldReturn` 0

    it "is not on Giant Wasp" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        m <- weakShocker p
        choose "attack" (isHeroAttack p)
        choose "Shocker" (isTargetChoice m)
        minionDamageOf m `shouldReturn` Nothing
        offers isSmallButMighty `shouldReturn` False

  describe "Giant Wasp" do
    it "may divide her basic attack among enemies" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        m <- addMinion p Minions.shocker
        v <- villainIdNow
        choose "attack" (isHeroAttack p)
        offers (isTargetChoice v) `shouldReturn` True
        choose "divide" (isLabel "$label.divideDamage")
        choose "the villain" (isTargetChoice v)
        choose "Shocker" (isTargetChoice m)
        villainDamageNow `shouldReturn` 1
        minionDamageOf m `shouldReturn` Just 1

    it "may divide her basic thwart among schemes" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        sid <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
        resolve [SideSchemeMessage sid (SideSchemePlaceThreat 2)]
        placeMainThreat 3
        side <- sideSchemeThreat sid
        mainBefore <- mainSchemeThreatNow
        choose "thwart" (isHeroThwart p)
        choose "divide" (isLabel "$label.divideThreat")
        choose "Bomb Scare" (isTargetChoice (SchemeSideSchemeId sid))
        choose "the main scheme" . isTargetChoice =<< mainSchemeIdNow
        sideSchemeThreat sid `shouldReturn` (side - 1)
        mainSchemeThreatNow `shouldReturn` (mainBefore - 1)

    it "Tiny Wasp does not divide" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        _ <- addMinion p Minions.shocker
        choose "attack" (isHeroAttack p)
        offers (isLabel "$label.divideDamage") `shouldReturn` False

  describe "G.I.R.L. (Nadia Van Dyne)" do
    it "shuffles up to 2 cards with a printed [mental] resource from the discard pile into the deck, once a round" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        [genius, chase, energy] <- stackDiscard p ["01089", "01052", "01088"]
        choose "G.I.R.L." (isNamedAbility "G.I.R.L.")
        offers (isTargetChoice energy) `shouldReturn` False
        choose "Genius" (isTargetChoice genius)
        offers (isTargetChoice genius) `shouldReturn` False
        choose "Chase Them Down" (isTargetChoice chase)
        discardOf p >>= (`shouldSatisfy` (\d -> genius `notElem` d && chase `notElem` d && energy `elem` d))
        deckOf p >>= (`shouldSatisfy` (\d -> genius `elem` d && chase `elem` d))
        offers (isNamedAbility "G.I.R.L.") `shouldReturn` False

    it "may shuffle only one, or none" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        [genius, chase] <- stackDiscard p ["01089", "01052"]
        choose "G.I.R.L." (isNamedAbility "G.I.R.L.")
        choose "Genius" (isTargetChoice genius)
        choose "done" (isLabel "$label.done")
        discardOf p >>= (`shouldSatisfy` (\d -> genius `notElem` d && chase `elem` d))
      tg' <- wasp
      let p' = player1 tg'
      inGame tg' do
        [genius] <- stackDiscard p' ["01089"]
        choose "G.I.R.L." (isNamedAbility "G.I.R.L.")
        choose "done" (isLabel "$label.done")
        discardOf p' >>= (`shouldSatisfy` elem genius)

  describe "Ant-Man (Scott Lang) (13002)" do
    it "gains Giant and +1 ATK in Giant form, Tiny and +1 THW in Tiny form, nothing in alter-ego" do
      tg <- wasp
      let p = player1 tg
          statsOf = do
            a <- allyIn "13002"
            (,,,) <$> Ally.getModifiedThwart a <*> Ally.getModifiedAttack a <*> allyMatches (AllyWithTrait Giant) (allyId a) <*> allyMatches (AllyWithTrait Tiny) (allyId a)
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "13002"
        statsOf `shouldReturn` (2, 2, False, False)
        changeFormTo p C
        pass
        statsOf `shouldReturn` (2, 3, True, False)
        resolve [IdentityMessage p (ChangedToForm A)]
        statsOf `shouldReturn` (3, 2, False, True)

  describe "Red Room Training (13008)" do
    it "in Giant form, she retaliates 1" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        _ <- putIntoPlay p "13008"
        rhinoAttacks p
        pass
        villainDamageNow `shouldReturn` 1

    it "in Tiny form, no retaliate" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        _ <- putIntoPlay p "13008"
        rhinoAttacks p
        pass
        villainDamageNow `shouldReturn` 0

    it "in Tiny form, her basic attack is piercing" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        _ <- putIntoPlay p "13008"
        v <- villainIdNow
        resolve [GiveStatus (toTarget v) ToughStatus]
        choose "attack" (isHeroAttack p)
        chooseVillain
        villainDamageNow `shouldReturn` 1
        villainIsTough <$> getVillain `shouldReturn` False

    it "in Giant form, her basic attack is not piercing" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        _ <- putIntoPlay p "13008"
        v <- villainIdNow
        resolve [GiveStatus (toTarget v) ToughStatus]
        choose "attack" (isHeroAttack p)
        chooseVillain
        villainDamageNow `shouldReturn` 0
        villainIsTough <$> getVillain `shouldReturn` False

  describe "Bio-Synthetic Wings (13009)" do
    it "Wasp is Aerial, Nadia Van Dyne is not" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "13009"
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` False
        changeFormTo p C
        pass
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` True
        resolve [IdentityMessage p (ChangedToForm A)]
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` True

    it "in Tiny form, exhausts to prevent 1 of the damage she would take" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p A
        wings <- putIntoPlay p "13009"
        runNow (damageIdentity p GameRef (toDamage 2 FromAbility))
        choose "Bio-Synthetic Wings" (isAbilityOfCard wings)
        pass
        damageOf p `shouldReturn` 1
        upgradeIsExhausted (upgradeIdOf wings) `shouldReturn` True

    it "not in Giant form" do
      tg <- wasp
      let p = player1 tg
      inGame tg do
        inForm p C
        wings <- putIntoPlay p "13009"
        runNow (damageIdentity p GameRef (toDamage 2 FromAbility))
        offers (isAbilityOfCard wings) `shouldReturn` False

  describe "Wasp's Helmet (13010)" do
    it "gives +1 THW in Giant form, +1 ATK in Tiny form" do
      tg <- wasp
      let p = player1 tg
          stats = (\s -> (s.thwart, s.attack)) <$> heroStats p
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "13010"
        changeFormTo p C
        pass
        stats `shouldReturn` (3, 2)
        resolve [IdentityMessage p (ChangedToForm A)]
        stats `shouldReturn` (1, 2)
