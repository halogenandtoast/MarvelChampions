module Marvel.Silk.Helpers where
import Marvel.Prelude
import Marvel.Window qualified as W
import Marvel.Matchers
import Marvel.Query
import Marvel.Card
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Id

matchingTuckedCards :: (HasGame m, IsRef a) => IdentityId -> a -> m [EncounterCard]
matchingTuckedCards you target = do
  def <- getsGame (`abilityCardDef` toTarget target)
  cards <- tuckedEncounterCards you
  pure [card | card <- cards, Just set <- [cdEncounterSet $ getCardDef card], (cdEncounterSet =<< def) == Just set]

chooseTuckedCard :: ReverseQueue m => IdentityId -> (EncounterCard -> m ()) -> m ()
chooseTuckedCard you body = do
  cards <- tuckedEncounterCards you
  chooseOne you [targeting card (body card) | card <- cards]

-- Replace the single pending reveal, leaving its source and all later work intact.
swapRevealingTuckedCard :: ReverseQueue m => IdentityId -> EncounterCard -> EncounterCard -> m ()
swapRevealingTuckedCard you incoming outgoing = do
  insteadOf pending $ do
    swapIdentityTuckedEncounterCard you incoming outgoing
    revealEncounterCard you outgoing
 where
  pending (CardSpecific GameRef key (fromJSON -> Success (_ :: W.RevealSource, _ :: IdentityId, card :: EncounterCard))) | key `elem` ["revealAfterWould", "revealAfterWouldWithoutWhenRevealed"] = ecCardId card == ecCardId incoming
  pending (RevealedEncounterCard _ card) = ecCardId card == ecCardId incoming
  pending (RevealedEncounterCardFrom _ _ card) = ecCardId card == ecCardId incoming
  pending _ = False

isScenarioSpecificEncounterCard :: HasGame m => EncounterCard -> m Bool
isScenarioSpecificEncounterCard card = do
  villains <- selectList AnyVillain
  schemes <- selectList MainScheme
  sets <- catMaybes <$> traverse (\target -> getsGame ((>>= cdEncounterSet) . (`abilityCardDef` target))) (map VillainRef villains <> map schemeRef schemes)
  pure $ maybe False (`elem` sets) (cdEncounterSet $ getCardDef card)
