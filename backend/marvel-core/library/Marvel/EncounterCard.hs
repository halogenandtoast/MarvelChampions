module Marvel.EncounterCard (
  module Marvel.EncounterCard,
  module X,
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Attachment.Cards
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard as X
import Marvel.EncounterSet
import Marvel.Minion.Cards
import Marvel.SideScheme.Cards
import Marvel.Treachery.Cards

allEncounterCards :: HashMap CardCode CardDef
allEncounterCards =
  allAttachments <> allMinions <> allTreacheries <> allSideSchemes <> allSpecialMinions

gatherEncounterSets ::
  (MonadRandom m) => HashSet EncounterSet -> m [EncounterCard]
gatherEncounterSets = concatMapM gatherEncounterSet . HashSet.toList

gatherEncounterSet :: (MonadRandom m) => EncounterSet -> m [EncounterCard]
gatherEncounterSet encounterSet =
  concat <$> for
    defs
    \def ->
      traverse genEncounterCard $
        replicate (fromIntegral $ fromMaybe 0 (cdEncounterSetQuantity def)) def
 where
  defs =
    filter ((== Just encounterSet) . cdEncounterSet) $ HashMap.elems allEncounterCards

genEncounterCard :: (MonadRandom m) => CardDef -> m EncounterCard
genEncounterCard cardDef = do
  cardId <- getRandom
  pure $ MkEncounterCard cardId cardDef
