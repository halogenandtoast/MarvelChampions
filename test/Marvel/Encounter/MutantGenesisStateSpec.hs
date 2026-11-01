module Marvel.Encounter.MutantGenesisStateSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Deck
import Marvel.Identity.Types
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

stackShapeshifter :: IdentityId -> CardDef -> GameAppT PlayerCard
stackShapeshifter you def = do
  encounter <- encounterCard def
  let card = encounterCardInPlayerDeck you encounter
  withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity attrs) ->
    PlayerIdentity attrs {playerIdentityDeck = Deck $ card : unDeck (playerIdentityDeck attrs)}
  pure card

spec :: Spec
spec = describe "Mutant Genesis shared rules" do
  describe "Shapeshifter encounter cards in player zones" do
    it "Infiltration stays in hand when drawn, discarding a controlled support instead of becoming a facedown encounter" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        mansion <- putIntoPlay p "01091"
        infiltration <- stackShapeshifter p Treacheries.mgInfiltration
        resolve [IdentityMessage p $ DrawCards FromDeck 1]
        handOf p >>= (`shouldSatisfy` elem infiltration)
        getsGame (HashMap.member (supportIdOf mansion) . gameSupports) `shouldReturn` False
        playerIdentityEncounterCards . toAttrs <$> getPlayer p `shouldReturn` []

    it "discarding Infiltration from the deck returns its original id to the encounter discard without drawing a replacement" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        infiltration <- stackShapeshifter p Treacheries.mgInfiltration
        resolve [IdentityMessage p $ DiscardFrom FromDeck 1 Nothing]
        handOf p `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` notElem infiltration)
        encounterDiscard >>= (`shouldSatisfy` any ((== toCardId infiltration) . toCardId))

    it "Infiltration can be discarded from hand and returns to the encounter discard pile" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        infiltration <- stackShapeshifter p Treacheries.mgInfiltration
        resolve [IdentityMessage p $ DrawCards FromDeck 1]
        resolve [DiscardedCard $ PlayerCard infiltration]
        handOf p `shouldReturn` []
        discardOf p `shouldReturn` []
        encounterDiscard >>= (`shouldSatisfy` any ((== toCardId infiltration) . toCardId))

  describe "Operation Zero Tolerance" do
    it "captures an attack-defeated ally before Rapid Response can retrieve it" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        cage <- revealNow p Schemes.mgOperationZeroTolerance
        rapid <- putIntoPlay p "08031"
        ally <- putIntoPlay p "01002"
        mid <- addMinion p Minions.hydraSoldier
        runNow [MinionMessage mid (MinionAttacks p)]
        choose "Black Cat defends" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
        let sid = SideSchemeId $ unCardId $ toCardId cage
        held <- getsGame (maybe [] (SideScheme.sideSchemeHeldCards . toAttrs) . HashMap.lookup sid . gameSideSchemes)
        liftIO $ map toCardId held `shouldContain` [toCardId ally]
        discardOf p >>= (`shouldSatisfy` notElem ally)
        chooseIf (isAbilityOfCard rapid)
        pass
        getsGame (HashMap.member (allyIdOf ally) . gameAllies) `shouldReturn` False
        heldAfter <- getsGame (maybe [] (SideScheme.sideSchemeHeldCards . toAttrs) . HashMap.lookup sid . gameSideSchemes)
        liftIO $ map toCardId heldAfter `shouldContain` [toCardId ally]
