module Marvel.Encounter.SpiderHamInheritorsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types qualified as Ability
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Keyword
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Phase (Phase (VillainPhase))
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Query
import Marvel.Window qualified as W
import TestImport

newtype InheritorModifiers = InheritorModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage InheritorModifiers where
  runMessage msg (InheritorModifiers attrs) = InheritorModifiers <$> runMessage msg attrs

modifyEntity :: EntityMatcher -> [Modifier] -> GameAppT ()
modifyEntity matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (InheritorModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = modifiers, effectEnds = Nothing })

minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs mid = getsGame (toAttrs . fromMaybe (error "missing minion") . HashMap.lookup mid . gameMinions)

keywordsOf :: MinionId -> GameAppT [Keyword]
keywordsOf mid = Minion.getModifiedKeywords =<< minionAttrs mid

attackOf :: MinionId -> GameAppT Natural
attackOf mid = Minion.getModifiedAttack =<< minionAttrs mid

settle :: GameAppT ()
settle = do
  pressWhenRevealed
  forced <- offers isForced
  if forced then choose "resolve the forced ability" isForced >> settle
    else pass >> whenM (offers isForced) settle
 where
  isForced = \case
    UseAbility a -> abilityType a `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse]
    _ -> False

asWebWarrior :: IdentityId -> GameAppT ()
asWebWarrior p = resolve [IdentityMessage p $ ChangedToForm A] >> pass >> clearHand p

revealMinion :: IdentityId -> CardDef -> GameAppT MinionId
revealMinion p def = do
  card <- revealNow p def
  settle
  pure $ MinionId $ unCardId $ ecCardId card

startHunt :: GameAppT ()
startHunt = runNow [CheckWindows [W.Window W.When $ W.PhaseBegan VillainPhase]] >> settle

spec :: Spec
spec = describe "Spider-Ham Inheritors modular" do
  describe "Hunting the Spider-Totems" do
    it "starts with six fixed threat with two players" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        card <- revealNow (player1 tg) Schemes.huntingTheSpiderTotems
        settle
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6

    it "discards three and puts only Inheritors into play engaged with a Web-Warrior controller" do
      tg <- testGame ["spider-man", "ghost-spider"]
      let p = player1 tg
          web = player2 tg
      inGame tg do
        asWebWarrior web
        _ <- revealNow p Schemes.huntingTheSpiderTotems
        settle
        (cards, _) <- setEncounterDeck [Minions.bora, Minions.hydraMercenary, Minions.brix, Minions.jennix] []
        startHunt
        let bora = MinionId $ unCardId $ ecCardId $ cards !! 0
            brix = MinionId $ unCardId $ ecCardId $ cards !! 2
        minionEngagedIdentity <$> minionAttrs bora `shouldReturn` web
        minionEngagedIdentity <$> minionAttrs brix `shouldReturn` web
        selectList (MinionIs Minions.hydraMercenary) `shouldReturn` []
        minionTough <$> minionAttrs brix `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` 0
        encounterDeckSize `shouldReturn` 1

    it "uses the first player when no player controls a Web-Warrior character" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Schemes.huntingTheSpiderTotems
        settle
        (cards, _) <- setEncounterDeck [Minions.jennix, Minions.brix, Minions.bora] []
        startHunt
        first <- selectJust FirstPlayer
        for_ cards \card -> minionEngagedIdentity <$> minionAttrs (MinionId $ unCardId $ ecCardId card) `shouldReturn` first
        minions <- selectList AnyMinion
        for_ minions \mid -> minionTough <$> minionAttrs mid `shouldReturn` False

    it "lets the first player choose between eligible Web-Warrior controllers" do
      tg <- testGame ["ghost-spider", "spider-man-miles-morales"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asWebWarrior p
        asWebWarrior other
        _ <- revealNow p Schemes.huntingTheSpiderTotems
        settle
        (cards, _) <- setEncounterDeck [Minions.jennix, Treacheries.toughItOut, Treacheries.toughItOut] []
        startHunt
        choose "engage Miles" (isTargetChoice other)
        settle
        minionEngagedIdentity <$> minionAttrs (MinionId $ unCardId $ ecCardId $ testHead cards) `shouldReturn` other

  describe "Inheritor auras" do
    for_ [("Brix", Minions.brix, Patrol), ("Daemos", Minions.daemos, Stalwart)
         ,("Jennix", Minions.jennix, Guard), ("Solus", Minions.solus, Villainous)
         ,("Verna", Minions.verna, Retaliate 1)] \(name, def, keyword) -> do
      it (name <> " grants its keyword to itself and other Inheritors, but not other minions") do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          source <- addMinion p def
          target <- addMinion p Minions.bora
          other <- addMinion p Minions.hydraBomber
          keywordsOf source >>= (`shouldSatisfy` elem keyword)
          keywordsOf target >>= (`shouldSatisfy` elem keyword)
          keywordsOf other >>= (`shouldSatisfy` notElem keyword)
          modifyEntity (MinionEntity $ MinionWithId source) [Blank]
          keywordsOf target >>= (`shouldSatisfy` notElem keyword)
      it (name <> " loses its aura when defeated") do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          source <- addMinion p def
          target <- addMinion p Minions.bora
          resolve [RemoveFromPlay $ MinionRef source]
          keywordsOf target >>= (`shouldSatisfy` notElem keyword)

    it "Bora adds an acceleration icon for every Inheritor and loses them on blanking" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        bora <- addMinion p Minions.bora
        getAccelerationCount `shouldReturn` 1
        _ <- addMinion p Minions.brix
        _ <- addMinion p Minions.hydraMercenary
        getAccelerationCount `shouldReturn` 2
        modifyEntity (MinionEntity $ MinionWithId bora) [Blank]
        getAccelerationCount `shouldReturn` 0

    it "Bora's acceleration icons leave play with him" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        bora <- addMinion p Minions.bora
        _ <- addMinion p Minions.brix
        getAccelerationCount `shouldReturn` 2
        resolve [RemoveFromPlay $ MinionRef bora]
        getAccelerationCount `shouldReturn` 0

    it "blanking Karn removes both granted attack keywords" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        karn <- addMinion p Minions.karn
        bora <- addMinion p Minions.bora
        modifyEntity (MinionEntity $ MinionWithId karn) [Blank]
        keywordsOf bora >>= (`shouldSatisfy` notElem Overkill)
        keywordsOf bora >>= (`shouldSatisfy` notElem Piercing)

    it "Karn grants overkill and piercing to all Inheritor attacks, until he leaves play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        karn <- addMinion p Minions.karn
        bora <- addMinion p Minions.bora
        other <- addMinion p Minions.hydraMercenary
        for_ [karn, bora] \mid -> do
          keywordsOf mid >>= (`shouldSatisfy` elem Overkill)
          keywordsOf mid >>= (`shouldSatisfy` elem Piercing)
        keywordsOf other >>= (`shouldSatisfy` notElem Overkill)
        keywordsOf other >>= (`shouldSatisfy` notElem Piercing)
        resolve [RemoveFromPlay $ MinionRef karn]
        keywordsOf bora >>= (`shouldSatisfy` notElem Overkill)
        keywordsOf bora >>= (`shouldSatisfy` notElem Piercing)

    it "Morlun gives all Inheritors +1 ATK, including himself, until blanked or removed" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        morlun <- addMinion p Minions.morlun
        bora <- addMinion p Minions.bora
        other <- addMinion p Minions.hydraMercenary
        attackOf morlun `shouldReturn` 3
        attackOf bora `shouldReturn` 2
        attackOf other `shouldReturn` 1
        modifyEntity (MinionEntity $ MinionWithId morlun) [Blank]
        attackOf bora `shouldReturn` 1
        resolve [RemoveFromPlay $ MinionRef morlun]
        attackOf bora `shouldReturn` 1

  describe "When revealed" do
    it "Bora places one threat on every scheme when any Web-Warrior character is in play" do
      tg <- testGame ["spider-man", "ghost-spider"]
      let p = player1 tg
      inGame tg do
        asWebWarrior (player2 tg)
        scheme <- revealNow p Schemes.huntingTheSpiderTotems
        settle
        _ <- revealMinion p Minions.bora
        mainSchemeThreatNow `shouldReturn` 1
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 7

    it "Brix places two threat on the main scheme when a Web-Warrior ally is in play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "27023"
        _ <- putIntoPlay p "27010"
        _ <- revealMinion p Minions.brix
        mainSchemeThreatNow `shouldReturn` 2

    it "Daemos lets the revealing player choose one of their characters to stun" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        asWebWarrior p
        ally <- putIntoPlay p "01084"
        _ <- revealNow p Minions.daemos
        choose "stun Mockingbird" (isTargetChoice $ allyIdOf ally)
        settle
        allyStunned <$> allyIn "01084" `shouldReturn` True
        playerIdentityStunned . toAttrs <$> getPlayer p `shouldReturn` False

    it "Daemos excludes already stunned characters and still offers an unstunned ally" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        asWebWarrior p
        stunned <- putIntoPlay p "01084"
        eligible <- putIntoPlay p "01002"
        resolve [GiveStatus (AllyRef $ allyIdOf stunned) StunnedStatus]
        _ <- revealNow p Minions.daemos
        offers (isTargetChoice $ allyIdOf stunned) `shouldReturn` False
        offers (isTargetChoice $ allyIdOf eligible) `shouldReturn` True
        choose "stun the eligible ally" (isTargetChoice $ allyIdOf eligible)
        settle
        allyStunned <$> allyIn "01002" `shouldReturn` True

    it "Daemos excludes stalwart characters while offering other controlled characters" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        asWebWarrior p
        eligible <- putIntoPlay p "01084"
        protected <- putIntoPlay p "01002"
        modifyEntity (IdentityEntity $ IdentityWithId p) [KeywordModifier Stalwart]
        modifyEntity (AllyEntity $ AllyWithId $ allyIdOf protected) [KeywordModifier Stalwart]
        _ <- revealNow p Minions.daemos
        offers (isTargetChoice p) `shouldReturn` False
        offers (isTargetChoice $ allyIdOf protected) `shouldReturn` False
        offers (isTargetChoice $ allyIdOf eligible) `shouldReturn` True
        choose "stun the eligible ally" (isTargetChoice $ allyIdOf eligible)
        settle
        playerIdentityStunned . toAttrs <$> getPlayer p `shouldReturn` False
        allyStunned <$> allyIn "01002" `shouldReturn` False
        allyStunned <$> allyIn "01084" `shouldReturn` True

    it "Daemos asks no empty question when every controlled character is already stunned or stalwart" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        asWebWarrior p
        ally <- putIntoPlay p "01084"
        resolve [GiveStatus (AllyRef $ allyIdOf ally) StunnedStatus]
        modifyEntity (IdentityEntity $ IdentityWithId p) [KeywordModifier Stalwart]
        _ <- revealMinion p Minions.daemos
        playerIdentityStunned . toAttrs <$> getPlayer p `shouldReturn` False
        allyStunned <$> allyIn "01084" `shouldReturn` True

    it "Jennix receives a tough status card when a Web-Warrior character is in play" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        asWebWarrior p
        jennix <- revealMinion p Minions.jennix
        minionTough <$> minionAttrs jennix `shouldReturn` True

    it "Karn discards a chosen upgrade or support belonging to the revealer" do
      tg <- testGame ["ghost-spider", "spider-man"]
      let p = player1 tg
      inGame tg do
        asWebWarrior p
        own <- putIntoPlay p "01092"
        _ <- putIntoPlay (player2 tg) "01092"
        _ <- revealNow p Minions.karn
        choose "discard my support" (isTargetChoice $ supportOf own)
        settle
        supportsWith "01092" >>= (`shouldSatisfy` (\xs -> length xs == 1))

    it "Morlun deals two damage to the player revealing him" do
      tg <- testGame ["ghost-spider", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asWebWarrior p
        _ <- revealMinion other Minions.morlun
        damageOf other `shouldReturn` 2
        damageOf p `shouldReturn` 0

    it "Solus receives one facedown boost card when a Web-Warrior character is in play" do
      tg <- testGame ["ghost-spider"]
      let p = player1 tg
      inGame tg do
        asWebWarrior p
        _ <- stackEncounterDeck [Treacheries.toughItOut]
        solus <- revealMinion p Minions.solus
        length . minionBoostCards <$> minionAttrs solus `shouldReturn` 1

    it "Verna damages every character controlled by the revealer, and no other player's characters" do
      tg <- testGame ["ghost-spider", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asWebWarrior p
        _ <- putIntoPlay p "01084"
        _ <- putIntoPlay other "01083"
        _ <- revealMinion p Minions.verna
        damageOf p `shouldReturn` 1
        allyDamage <$> allyIn "01084" `shouldReturn` 1
        damageOf other `shouldReturn` 0
        allyDamage <$> allyIn "01083" `shouldReturn` 0

    for_ [("Bora", Minions.bora), ("Brix", Minions.brix), ("Daemos", Minions.daemos)
         ,("Jennix", Minions.jennix), ("Karn", Minions.karn), ("Morlun", Minions.morlun)
         ,("Solus", Minions.solus), ("Verna", Minions.verna)] \(name, def) ->
      it (name <> " does not apply its reveal effect without a Web-Warrior character") do
        tg <- testGame ["spider-man"]
        let p = player1 tg
        inGame tg do
          _support <- putIntoPlay p "01092"
          mid <- revealMinion p def
          mainSchemeThreatNow `shouldReturn` 0
          damageOf p `shouldReturn` 0
          playerIdentityStunned . toAttrs <$> getPlayer p `shouldReturn` False
          minionTough <$> minionAttrs mid `shouldReturn` False
          length . minionBoostCards <$> minionAttrs mid `shouldReturn` 0
          supportsWith "01092" >>= (`shouldSatisfy` (\xs -> length xs == 1))
