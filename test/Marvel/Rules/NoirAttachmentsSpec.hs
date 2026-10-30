module Marvel.Rules.NoirAttachmentsSpec (spec) where

import Data.Aeson (Result (..), Value (..), fromJSON, toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Game.Public (publicGame)
import Marvel.Minion.Types qualified as Minion
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

captureTreachery :: IdentityId -> GameAppT (PlayerCard, EncounterCard)
captureTreachery who = do
  changeForm who
  pass
  noir <- putIntoPlay who "31015"
  treachery <- revealNow who Treacheries.imTough
  choose "Spider-Man Noir captures the resolved treachery" (isAbilityOfCard noir)
  pass
  allyHeldTreacheries <$> allyIn "31015" `shouldReturn` [treachery]
  getEncounterDiscard >>= (`shouldSatisfy` notElem treachery)
  pure (noir, treachery)

convertedAttrs :: MinionId -> GameAppT (Attrs Minion)
convertedAttrs mid = getsGame $ maybe (error "expected converted Noir") toAttrs . HashMap.lookup mid . gameMinions

-- The client receives only the number of facedown cards, not their identities.
shownMinionAttachments :: GameAppT [(Int, Int)]
shownMinionAttachments = do
  view <- liftIO . publicGame =<< getGame
  pure
    [ (floor n, length cards)
    | Object game <- [view]
    , Just (Array minions) <- [KeyMap.lookup "minions" game]
    , Object minion <- toList minions
    , Just (Number n) <- [KeyMap.lookup "facedownCards" minion]
    , Just (Array cards) <- [KeyMap.lookup "heldCards" minion]
    ]

spec :: Spec
spec = describe "Noir attachments when an ally becomes a minion" do
  it "keeps the facedown treachery through both conversions and restores Noir's printed ability and power" do
    tg <- testGame ["spdr"]
    let who = player1 tg
    inGame tg do
      (noir, treachery) <- captureTreachery who
      let aid = allyIdOf noir
          mid = MinionId $ unAllyId aid
      resolve [AllyBecomesMinion aid]
      Minion.minionHeldTreacheries <$> convertedAttrs mid `shouldReturn` [treachery]
      getEncounterDiscard >>= (`shouldSatisfy` notElem treachery)
      shownMinionAttachments `shouldReturn` [(1, 0)]
      -- The converted ally has a blank text box. Its attachment stays in play;
      -- it does not grant the absent Noir ability while he is a minion.
      (Minion.getModifiedAttack =<< convertedAttrs mid) `shouldReturn` 0
      resolve [MinionBecomesAlly mid]
      attrs <- allyIn "31015"
      Ally.allyHeldTreacheries attrs `shouldBe` [treachery]
      Ally.getModifiedAttack attrs `shouldReturn` 1
      Ally.getModifiedThwart attrs `shouldReturn` 1
      getEncounterDiscard >>= (`shouldSatisfy` notElem treachery)
      -- A real departure after returning discards the attachment exactly once.
      resolve [RemoveFromPlay $ toRef aid]
      length . filter (== treachery) <$> getEncounterDiscard `shouldReturn` 1

  for_ [ ("discard", \mid -> RemoveFromPlay $ toRef mid)
       , ("remove from game", \mid -> RemoveFromGame $ toRef mid)
       , ("victory display", \mid -> AddToVictoryDisplay $ toRef mid)
       , ("defeat", \mid -> MinionMessage mid MinionDefeated)
       ] \(route, leave) ->
    it ("discards its facedown treachery once when the converted minion leaves via " <> route) do
      tg <- testGame ["spdr"]
      let who = player1 tg
      inGame tg do
        (noir, treachery) <- captureTreachery who
        let mid = MinionId $ unAllyId $ allyIdOf noir
        resolve [AllyBecomesMinion $ allyIdOf noir]
        resolve [leave mid]
        getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
        length . filter (== treachery) <$> getEncounterDiscard `shouldReturn` 1
        -- A repeated departure message cannot discard the same attachment again.
        resolve [RemoveFromPlay $ toRef mid]
        length . filter (== treachery) <$> getEncounterDiscard `shouldReturn` 1

  it "round-trips attached treacheries in saves and loads older minions with none" do
    tg <- testGame ["spdr"]
    let who = player1 tg
    inGame tg do
      (noir, treachery) <- captureTreachery who
      let mid = MinionId $ unAllyId $ allyIdOf noir
      resolve [AllyBecomesMinion $ allyIdOf noir]
      minion <- getsGame $ fromMaybe (error "missing Noir") . HashMap.lookup mid . gameMinions
      case fromJSON @Minion (toJSON minion) of
        Error err -> fail err
        Success loaded -> Minion.minionHeldTreacheries (toAttrs loaded) `shouldBe` [treachery]
      let old = case toJSON minion of
            Object o -> Object $ KeyMap.delete "minionHeldTreacheries" o
            other -> other
      case fromJSON @Minion old of
        Error err -> fail err
        Success loaded -> Minion.minionHeldTreacheries (toAttrs loaded) `shouldBe` []
