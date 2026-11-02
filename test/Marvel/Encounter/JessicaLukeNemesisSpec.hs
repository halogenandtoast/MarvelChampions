module Marvel.Encounter.JessicaLukeNemesisSpec (spec) where

import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.JessicaJones.Helpers qualified as JL
import Marvel.Game.Source qualified as Engine
import Marvel.Obligation.Types qualified as Obligation
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Status
import Data.HashMap.Strict qualified as HashMap
import Marvel.Phase
import TestImport

isObligationAction :: Choice -> Bool
isObligationAction = \case
  UseAbility ab -> case abilitySource ab of
    ObligationRef _ -> True
    _ -> False
  _ -> False

spec :: Spec
spec = describe "Jessica Jones and Luke Cage nemeses" do
  it "Work-Life Balance stays in play and adds a discard cost to changing form" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.jl61030
      pass
      obligationsInPlay `shouldReturn` 1
      modifiers <- Engine.getModifiers p
      modifiers `shouldSatisfy` any (\case AdditionalChangeFormCost _ -> True; _ -> False)

  it "Work-Life Balance can be discarded by confusing Jessica in alter ego" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.jl61030
      pass
      choose "obligation action" isObligationAction
      choose "confuse Jessica" (isLabel "$label.workLifeConfuse")
      isConfused p `shouldReturn` True
      obligationsInPlay `shouldReturn` 0

  it "Internal Injury deals three unpreventable damage without removing tough" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.jl62028
      pass
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      clearHand p
      start [EndPhase PlayerPhase]
      choose "Internal Injury forced response" isObligationAction
      damageOf p `shouldReturn` 3
      JL.toughCount p `shouldReturn` 1

  it "Suggestion enters with three pheromone counters" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      _ <- revealNow (player1 tg) Cards.jl61033a
      pass
      obligations <- getsGame (HashMap.elems . gameObligations)
      map Obligation.getObligationDisplayCounters obligations `shouldBe` [3]

  it "Suggestion can place two threat, consumes a counter and exhausts" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- revealNow p Cards.jl61033a
      pass
      before <- mainSchemeThreatNow
      choose "Suggestion" isObligationAction
      choose "place threat" (isLabel "$label.suggestionThreat")
      mainSchemeThreatNow `shouldReturn` (before + 2)
      obligations <- getsGame (HashMap.elems . gameObligations)
      map Obligation.getObligationDisplayCounters obligations `shouldBe` [2]
      map Obligation.getObligationDisplayExhausted obligations `shouldBe` [True]

  it "Indomitable Will adds two counters to a newly revealed Suggestion" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.jl61032
      pass
      _ <- revealNow p Cards.jl61033b
      pass
      obligations <- getsGame (HashMap.elems . gameObligations)
      map Obligation.getObligationDisplayCounters obligations `shouldBe` [5]

  it "Purple Man copies the villain's greater ATK and SCH" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.jl61031
      minion <- getsGame (fromMaybe (error "missing Purple Man") . HashMap.lookup mid . gameMinions)
      Minion.getModifiedAttack (toAttrs minion) `shouldReturn` 2
      Minion.getModifiedScheme (toAttrs minion) `shouldReturn` 1

  it "Purple Man uses Jessica's three THW for his SCH in hero form" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      mid <- addMinion p Cards.jl61031
      minion <- getsGame (fromMaybe (error "missing Purple Man") . HashMap.lookup mid . gameMinions)
      Minion.getModifiedScheme (toAttrs minion) `shouldReturn` 3

  it "Venomous Whispers protects Serpent Society minions from attacks" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.jl62029
      _ <- revealNow p Cards.jl62030
      pass
      modifiers <- Engine.getModifiers (MinionRef mid)
      modifiers `shouldSatisfy` elem CannotBeAttacked

  it "Augmented Jaws attaches to Cottonmouth and increases his ATK" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.jl62029
      _ <- revealNow p Cards.jl62031
      pass
      minion <- getsGame (fromMaybe (error "missing Cottonmouth") . HashMap.lookup mid . gameMinions)
      Minion.getModifiedAttack (toAttrs minion) `shouldReturn` 2

  it "Augmented Jaws attaches to the villain when Cottonmouth is absent" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      _ <- revealNow (player1 tg) Cards.jl62031
      pass
      villain <- villainIdNow
      modifiers <- Engine.getModifiers (VillainRef villain)
      modifiers `shouldSatisfy` elem (AttackModifier 1)

  it "the physical Suggestion permits only a printed physical resource card" do
    tg <- testGame ["jessica-jones"]
    inGame tg do
      let p = player1 tg
      clearHand p
      physical <- putInHand p "61011"
      _ <- putInHand p "01088"
      mental <- putInHand p "61005"
      _ <- revealNow p Cards.jl61033c
      pass
      choose "Suggestion" isObligationAction
      offers (isTargetChoice physical) `shouldReturn` True
      offers (isTargetChoice mental) `shouldReturn` False

  it "Asp stuns the player after scheming" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.jl62032
      resolve [MinionMessage mid $ MinionSchemesAgainst p]
      isStunned p `shouldReturn` True

  it "Sidewinder becomes a facedown encounter card if the discarded card has a boost ability" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      mid <- addMinion p Cards.jl62033
      _ <- setEncounterDeck [Cards.jl62029] []
      choose "basic attack" (isHeroAttack p)
      choose "Sidewinder" (isTargetChoice $ EnemyMinionId mid)
      choose "Sidewinder forced interrupt" (\case UseAbility ab -> abilitySource ab == MinionRef mid && abilityIndex ab == 1; _ -> False)
      pass
      minionDamageOf mid `shouldReturn` Nothing
      cards <- map toCardCode <$> dealtTo p
      cards `shouldSatisfy` elem "62033"

  it "Cottonmouth's extra attacks stop when the original ally target is defeated" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.jl62029
      ally <- putIntoPlay p "62002"
      resolve [AllyMessage (allyIdOf ally) $ AllyDamaged (toSource p) $ toDamage 1 FromAbility]
      start [MinionMessage mid $ MinionAttacksAlly p (allyIdOf ally)]
      choose "first attack undefended" isNoDefender
      choose "Cottonmouth attacks twice more" (\case UseAbility ab -> abilitySource ab == MinionRef mid && abilityIndex ab == 1; _ -> False)
      choose "second attack undefended" isNoDefender
      pass
      alliesWith "62002" `shouldReturn` []
      damageOf p `shouldReturn` 0

  it "Indomitable Will has two threat per player plus Hinder three per player" do
    tg <- testGame ["jessica-jones", "luke-cage"]
    inGame tg do
      encounter <- revealNow (player1 tg) Cards.jl61032
      pass
      sideSchemeThreat (sideSchemeOf encounter) `shouldReturn` 10

  it "Venomous Whispers starts with three threat per player" do
    tg <- testGame ["jessica-jones", "luke-cage"]
    inGame tg do
      encounter <- revealNow (player1 tg) Cards.jl62030
      pass
      sideSchemeThreat (sideSchemeOf encounter) `shouldReturn` 6
