module Marvel.Encounter.JubileeNemesisSpec (spec) where
import TestImport
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability qualified as A
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Minion.Types qualified as M
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Trait (Trait(..))
import Marvel.Cost.Types
import Marvel.ActiveCost.Types (ActiveCost(..))

asJubilee :: IdentityId -> GameAppT ()
asJubilee p = changeForm p >> pass >> clearHand p
settleForced :: GameAppT ()
settleForced = do
  question <- asked
  case question >>= (headMay . filter forced . snd) of
    Just choice -> choose "forced response" (== choice) >> settleForced
    Nothing -> pass
 where
  forced = \case UseAbility ab -> abilityType ab `elem` [A.ForcedResponse,A.ForcedInterrupt]; _ -> False
minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs mid = getsGame (maybe (error "missing minion") toAttrs . HashMap.lookup mid . gameMinions)

spec :: Spec
spec = describe "Jubilee nemesis and Arcade" do
  it "Grounded changes Jubilee to alter ego and remains in play" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      _ <- revealNow p Cards.jubilee47023
      pass
      identityMatches AlterEgoIdentity p `shouldReturn` True
      mods <- getModifiers p
      mods `shouldSatisfy` elem (AdditionalChangeToHeroFormCost $ SameResourcesCost 2)

  it "Grounded is removed after a Jubilee event is played" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      obligation <- revealNow p Cards.jubilee47023
      pass
      formPayment <- putInHand p "01088"
      runNow [IdentityMessage p $ ChangedToForm A]
      payWith [formPayment]
      pass
      card <- putInHand p "47007a"
      resources <- putInHand p "47010a"
      playWith card [resources]
      chooseVillain
      choose "remove Grounded" \case
        UseAbility ab -> abilitySource ab == ObligationRef (ObligationId $ unCardId $ ecCardId obligation)
        _ -> False
      pass
      mods <- getModifiers p
      mods `shouldSatisfy` notElem (AdditionalChangeToHeroFormCost $ SameResourcesCost 2)

  it "Nanny enters tough and finds Lost Child after attacking a player with an ally" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      ally <- putIntoPlay p "47011"
      nanny <- revealNow p Cards.jubilee47024
      pass
      let mid = MinionId $ unCardId $ ecCardId nanny
      minionTough <$> minionAttrs mid `shouldReturn` True
      _ <- setEncounterDeck [Cards.jubilee47027, Treacheries.advance] []
      runNow [MinionMessage mid $ MinionAttacks p]
      choose "take Nanny's attack" isNoDefender
      settleForced
      let converted = MinionId $ unAllyId $ allyIdOf ally
      selectAny (MinionWithId converted) `shouldReturn` True
      alliesWith "47011" `shouldReturn` []

  it "Nanny does not search for Lost Child without allies" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      nanny <- revealNow p Cards.jubilee47024
      pass
      let mid = MinionId $ unCardId $ ecCardId nanny
      _ <- setEncounterDeck [Cards.jubilee47027,Treacheries.advance] []
      runNow [MinionMessage mid $ MinionAttacks p]
      choose "take attack" isNoDefender
      settleForced
      selectAny (MinionWithTrait Regressed) `shouldReturn` False

  it "Naughty Children counts different printed resource types in the revealing hand" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- putInHand p "47010a"
      _ <- putInHand p "47010b"
      _ <- putInHand p "47004"
      scheme <- revealNow p Cards.jubilee47025
      pass
      sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 6

  it "Battle Suit attaches to the minion with fewest remaining HP, adding ATK, HP, Aerial" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      weak <- addMinion p Minions.hydraRegular
      _ <- addMinion p Minions.laufey
      _ <- revealNow p Cards.jubilee47026
      pass
      mods <- getModifiers (MinionRef weak)
      mods `shouldSatisfy` (\xs -> all (`elem` xs) [AttackModifier 1, HitPointModifier 3,TraitModifier Aerial])

  it "Lost Child converts the highest-cost ally, blanks it, and reduces SCH by one" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "47018"
      high <- putIntoPlay p "47011"
      card <- revealNow p Cards.jubilee47027
      pass
      let mid = MinionId $ unAllyId $ allyIdOf high
      selectAny (MinionWithId mid <> MinionWithTrait Regressed) `shouldReturn` True
      (M.getModifiedScheme =<< minionAttrs mid) `shouldReturn` 1
      mods <- getModifiers (MinionRef mid)
      mods `shouldSatisfy` elem Blank
      length <$> alliesWith "47018" `shouldReturn` 1
      resolve [RemoveFromPlay $ AttachmentRef $ AttachmentId $ unCardId $ ecCardId card]
      pass
      length <$> alliesWith "47011" `shouldReturn` 1
      selectAny (MinionWithId mid) `shouldReturn` False

  it "Arcade reveals a Trap by discarding and cannot take damage while it remains" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      asJubilee p
      _ <- setEncounterDeck [Treacheries.advance, Cards.jubilee47031] []
      arcade <- revealNow p Cards.jubilee47030
      pass
      let mid = MinionId $ unCardId $ ecCardId arcade
      trap <- selectOne $ SideSchemeIs Cards.jubilee47031
      trap `shouldSatisfy` isJust
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 2 FromAbility]
      minionDamage <$> minionAttrs mid `shouldReturn` 0
      for_ trap $ \sid -> resolve [RemoveFromPlay $ SideSchemeRef sid]
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 2 FromAbility]
      minionDamage <$> minionAttrs mid `shouldReturn` 2

  it "Welcome to Murderworld has hinder and damages its defeating player" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      trap <- revealNow p Cards.jubilee47031
      pass
      let sid = sideSchemeOf trap
      sideSchemeThreat sid `shouldReturn` 3
      runNow [SideSchemeMessage sid $ SideSchemeThreatRemoved (IdentityRef p) 3]
      pass
      damageOf p `shouldReturn` 2

  it "Arcade's Funhouse stuns, and when already stunned discards an upgrade" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      trap <- revealNow p Cards.jubilee47032
      pass
      let sid = sideSchemeOf trap
      runNow [SideSchemeMessage sid $ SideSchemeThreatRemoved (IdentityRef p) 3]
      pass
      isStunned p `shouldReturn` True
      upgrade <- putIntoPlay p "47004"
      trap2 <- revealNow p Cards.jubilee47032
      pass
      runNow [SideSchemeMessage (sideSchemeOf trap2) $ SideSchemeThreatRemoved (IdentityRef p) 3]
      choose "discard the coat" (isTargetChoice $ upgradeIdOf upgrade)
      pass
      upgradeCodes `shouldReturn` []

  it "Hall of Mirrors confuses, and when already confused places two threat" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      trap <- revealNow p Cards.jubilee47033
      pass
      runNow [SideSchemeMessage (sideSchemeOf trap) $ SideSchemeThreatRemoved (IdentityRef p) 3]
      pass
      isConfused p `shouldReturn` True
      before <- mainSchemeThreatNow
      trap2 <- revealNow p Cards.jubilee47033
      pass
      runNow [SideSchemeMessage (sideSchemeOf trap2) $ SideSchemeThreatRemoved (IdentityRef p) 3]
      pass
      mainSchemeThreatNow `shouldReturn` before + 2

  it "Elaborate Trap resolves every trap's defeat ability without removing those schemes" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      one <- revealNow p Cards.jubilee47031
      pass
      two <- revealNow p Cards.jubilee47033
      pass
      _ <- revealNow p Cards.jubilee47034
      pass
      damageOf p `shouldReturn` 2
      isConfused p `shouldReturn` True
      sideSchemeThreat (sideSchemeOf one) `shouldReturn` 3
      sideSchemeThreat (sideSchemeOf two) `shouldReturn` 3

  it "Elaborate Trap reveals a discarded Trap when no defeat abilities could resolve" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Treacheries.advance,Cards.jubilee47032] []
      _ <- revealNow p Cards.jubilee47034
      pass
      selectAny (SideSchemeIs Cards.jubilee47032) `shouldReturn` True

  it "Grounded revealed by another player is given to Jubilee" do
    tg <- testGame ["jubilee","spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      asJubilee p
      _ <- revealNow other Cards.jubilee47023
      pass
      identityMatches AlterEgoIdentity p `shouldReturn` True
      mods <- getModifiers p
      mods `shouldSatisfy` elem (AdditionalChangeToHeroFormCost $ SameResourcesCost 2)
      otherMods <- getModifiers other
      otherMods `shouldSatisfy` notElem (AdditionalChangeToHeroFormCost $ SameResourcesCost 2)

  it "Grounded requires two resources of the same type before changing to hero" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- revealNow p Cards.jubilee47023
      pass
      energy <- putInHand p "01088"
      changeForm p
      identityMatches AlterEgoIdentity p `shouldReturn` True
      payWith [energy]
      pass
      identityMatches HeroIdentity p `shouldReturn` True
      discard <- discardOf p
      discard `shouldSatisfy` elem energy

  it "Grounded does not charge for a form change during another player's turn" do
    tg <- testGame ["spider-man","jubilee"]
    inGame tg do
      let other = player1 tg
          p = player2 tg
      _ <- revealNow other Cards.jubilee47023
      pass
      clearHand p
      resolve [IdentityMessage p $ ChangedToForm A]
      identityMatches HeroIdentity p `shouldReturn` True

  it "Battle Suit surges instead of staying unattached when no minion is in play" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Minions.hydraRegular, Treacheries.advance] []
      _ <- revealNow p Cards.jubilee47026
      pass
      remaining <- selectAny (AttachmentIs Cards.jubilee47026)
      remaining `shouldBe` False
      dealt <- playerIdentityEncounterCards <$> playerAttrs p
      map toCardCode dealt `shouldBe` [toCardCode Minions.hydraRegular]
      selectAny (MinionIs Minions.hydraRegular) `shouldReturn` False

  it "Lost Child surges when there is no ally to attach to" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Minions.hydraRegular, Treacheries.advance] []
      _ <- revealNow p Cards.jubilee47027
      pass
      remaining <- selectAny (AttachmentIs Cards.jubilee47027)
      remaining `shouldBe` False
      dealt <- playerIdentityEncounterCards <$> playerAttrs p
      map toCardCode dealt `shouldBe` [toCardCode Minions.hydraRegular]
      selectAny (MinionIs Minions.hydraRegular) `shouldReturn` False

  it "Grounded does not offer a normal form change with only two different resource types" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.jubilee47023
      pass
      clearHand p
      _ <- putInHand p "47010a"
      offers (\case UseAbility ab -> ChangeForm `elem` abilityChoices ab; _ -> False) `shouldReturn` False

  it "aborting Grounded's unpaid normal form change does not consume the once-per-turn use" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.jubilee47023
      pass
      clearHand p
      energy <- putInHand p "01088"
      changeForm p
      pending <- getsGame (headMay . gameActiveCost)
      case pending of
        Nothing -> fail "expected the form-change payment"
        Just cost -> do
          withGame_ $ questionL .~ mempty
          push $ AbortPayment $ activeCostId cost
          runGame
      pass
      changeForm p
      payWith [energy]
      pass
      identityMatches HeroIdentity p `shouldReturn` True
