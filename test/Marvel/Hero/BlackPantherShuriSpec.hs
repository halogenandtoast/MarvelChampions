module Marvel.Hero.BlackPantherShuriSpec (spec) where
import TestImport
import Marvel.Card.Types (toCard)
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Query
import Marvel.Message.Lifted qualified as L
import Marvel.Modifier (Modifier(CannotBeHealedByPlayerCards))
import Marvel.Minion.Cards qualified as Minions

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

pay :: IdentityId -> CardCode -> GameAppT PlayerCard
pay p code = do
  card <- putInHand p code
  resource <- putInHand p "51006"
  playWith card [resource]
  pure card

spec :: Spec
spec = describe "Black Panther Shuri" do
  it "Shuri starts with six cards and eleven hit points" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      handSizeOf (player1 tg) `shouldReturn` 6
      hitPointsOf (player1 tg) `shouldReturn` 11

  it "Black Panther changes to a five-card hero" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      asHero (player1 tg)
      handSizeOf (player1 tg) `shouldReturn` 5

  it "Clawed Strike deals four damage without requiring an upgrade" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- pay p "51003"
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 4

  it "On the Prowl removes three threat without requiring an upgrade" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      _ <- pay p "51004"
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 3

  it "Clawed Strike resolves Panther Claws after its attack" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      claws <- putIntoPlay p "51011"
      _ <- pay p "51003"
      chooseVillain
      choose "Panther Claws Special" (isTargetChoice $ upgradeIdOf claws)
      chooseVillain
      choose "keep Panther Claws" (isLabel "$label.done")
      pass
      villainDamageNow `shouldReturn` 6

  it "Panther Claws can be discarded for one five-damage piercing attack" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      claws <- putIntoPlay p "51011"
      v <- villainIdNow
      resolve [GiveStatus (VillainRef v) ToughStatus]
      card <- putInHand p "51005"
      payment <- putInHand p "01093"
      playWith card [payment]
      choose "Panther Claws Special" (isTargetChoice $ upgradeIdOf claws)
      chooseVillain
      choose "discard claws" (isTargetChoice $ upgradeIdOf claws)
      pass
      villainDamageNow `shouldReturn` 5
      selectAny (UpgradeIs Cards.pantherClawsShuri) `shouldReturn` False

  it "Queen Ramonda heals the chosen Wakandan alter-ego by its REC" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      ramonda <- putIntoPlay p "51008"
      setDamage p 7
      choose "Queen Ramonda" (isAbilityOfCard ramonda)
      choose "Shuri" (isTargetChoice p)
      pass
      damageOf p `shouldReturn` 3

  it "The Elephant's Trunk draws one card when no additional card is exhausted" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      trunk <- putIntoPlay p "51007"
      choose "The Elephant's Trunk" (isAbilityOfCard trunk)
      choose "no extra cards" (isLabel "$label.done")
      pass
      (length <$> handOf p) `shouldReturn` 1

  it "Aja-Adanna shuffles a signature card from the discard pile" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      aja <- putIntoPlay p "51009"
      card <- putInHand p "51003"
      resolve [DiscardedCard $ toCard card]
      choose "Aja-Adanna" (isAbilityOfCard aja)
      choose "Clawed Strike" (isTargetChoice card)
      pass
      (any ((== pcCardId card) . pcCardId) <$> discardOf p) `shouldReturn` False

  it "Inventor finds and plays a two-cost upgrade from the deck without resource payment" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cards <- stackDeck p ["51010"]
      choose "Inventor" (\case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False)
      choose "Kimoyo Beads" (isTargetChoice $ testHead cards)
      pass
      selectAny (UpgradeIs Cards.kimoyoBeadsShuri) `shouldReturn` True
      exhaustedNow p `shouldReturn` True

  it "Kimoyo Beads removes one threat and can be retained" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      beads <- putIntoPlay p "51010"
      placeMainThreat 3
      before <- mainSchemeThreatNow
      card <- putInHand p "51005"
      payment <- putInHand p "01093"
      playWith card [payment]
      choose "Kimoyo Beads Special" (isTargetChoice $ upgradeIdOf beads)
      chooseMainScheme
      choose "retain beads" (isLabel "$label.done")
      pass
      mainSchemeThreatNow `shouldReturn` before - 1
      selectAny (UpgradeIs Cards.kimoyoBeadsShuri) `shouldReturn` True

  it "Kimoyo Beads can be discarded to confuse after its threat removal" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      beads <- putIntoPlay p "51010"
      placeMainThreat 3
      card <- putInHand p "51005"
      payment <- putInHand p "01093"
      playWith card [payment]
      choose "Kimoyo Beads Special" (isTargetChoice $ upgradeIdOf beads)
      chooseMainScheme
      chooseVillain
      pass
      v <- villainIdNow
      enemyMatches ConfusedEnemy (EnemyVillainId v) `shouldReturn` True
      selectAny (UpgradeIs Cards.kimoyoBeadsShuri) `shouldReturn` False

  it "Spider Bites damages the villain and each minion engaged with the chosen player" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      bites <- putIntoPlay p "51012"
      card <- putInHand p "51005"
      payment <- putInHand p "01093"
      playWith card [payment]
      choose "Spider Bites Special" (isTargetChoice $ upgradeIdOf bites)
      choose "chosen player" (isTargetChoice p)
      choose "keep Spider Bites" (isLabel "$label.done")
      pass
      villainDamageNow `shouldReturn` 1

  it "Vibranium Suit moves one damage from the hero to an enemy" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      setDamage p 3
      suit <- putIntoPlay p "51013"
      card <- putInHand p "51005"
      payment <- putInHand p "01093"
      playWith card [payment]
      choose "Vibranium Suit Special" (isTargetChoice $ upgradeIdOf suit)
      chooseVillain
      choose "keep suit" (isLabel "$label.done")
      pass
      damageOf p `shouldReturn` 2
      villainDamageNow `shouldReturn` 1

  it "Black Panther's basic power response resolves one controlled upgrade Special" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      claws <- putIntoPlay p "51011"
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      choose "Black Panther response" (\case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False)
      choose "Panther Claws Special" (isTargetChoice $ upgradeIdOf claws)
      chooseVillain
      choose "retain claws" (isLabel "$label.done")
      pass
      villainDamageNow `shouldReturn` 3

  it "T'Challa's basic power response resolves an upgrade Special" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      claws <- putIntoPlay p "51011"
      tChalla <- putIntoPlay p "51002"
      choose "T'Challa attacks" (\case UseAbility a -> abilitySource a == AllyRef (allyIdOf tChalla) && abilityIndex a == 301; _ -> False)
      chooseVillain
      choose "T'Challa response" (isAbilityOfCard tChalla)
      choose "Panther Claws Special" (isTargetChoice $ upgradeIdOf claws)
      chooseVillain
      choose "retain claws" (isLabel "$label.done")
      pass
      villainDamageNow `shouldReturn` 4

  it "Inventor remains limited once per round even if Shuri is readied" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      clearHand p
      cards <- stackDeck p ["51010","51011"]
      choose "Inventor" (\case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False)
      choose "Kimoyo Beads" (isTargetChoice $ testHead cards)
      pass
      resolve [IdentityMessage p ReadiedIdentity]
      offers (\case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False) `shouldReturn` False

  it "Vibranium Suit may be discarded to give Black Panther tough" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      suit <- putIntoPlay p "51013"
      card <- putInHand p "51005"
      payment <- putInHand p "01093"
      playWith card [payment]
      choose "Vibranium Suit Special" (isTargetChoice $ upgradeIdOf suit)
      choose "discard suit" (isTargetChoice $ upgradeIdOf suit)
      pass
      (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

  it "Vibranium Suit cannot move damage from an identity that player cards cannot heal" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      suit <- putIntoPlay p "51013"
      setDamage p 2
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (IdentityEntity $ IdentityWithId p) [CannotBeHealedByPlayerCards] True) >>= runNow
      card <- putInHand p "51005"
      payment <- putInHand p "01093"
      playWith card [payment]
      choose "Vibranium Suit Special" (isTargetChoice $ upgradeIdOf suit)
      offers (\case TargetLabel (VillainRef _) _ -> True; _ -> False) `shouldReturn` False
      choose "discard suit for tough" (isTargetChoice $ upgradeIdOf suit)
      pass
      damageOf p `shouldReturn` 2
      villainDamageNow `shouldReturn` 0
      (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

  it "T'Challa can resolve an upgrade Special after defending" do
    tg <- testGame ["black-panther-shuri"]
    inGame tg do
      let p = player1 tg
      asHero p
      claws <- putIntoPlay p "51011"
      ally <- putIntoPlay p "51002"
      mid <- addMinion p Minions.hydraBomber
      L.runQueueT (L.capture $ L.enemyAttacksAlly mid p (allyIdOf ally)) >>= start
      choose "ally defends" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
      choose "T'Challa's defense response" (isAbilityOfCard ally)
      choose "Panther Claws" (isTargetChoice $ upgradeIdOf claws)
      chooseVillain
      choose "retain claws" (isLabel "$label.done")
      pass
      villainDamageNow `shouldReturn` 2
