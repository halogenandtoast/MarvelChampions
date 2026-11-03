module Marvel.EncounterCard (
  module Marvel.EncounterCard,
  module X,
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text qualified as Text
import Marvel.Ally.Cards qualified as Allies
import Marvel.CivilWar.Cards qualified as CW
import Marvel.Attachment.Cards
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard as X
import Marvel.EncounterSet
import Marvel.Environment.Cards qualified as Environments
import Marvel.Minion.Cards
import Marvel.Obligation.Cards
import Marvel.SideScheme.Cards
import Marvel.Support.Cards (allEncounterSupports)
import Marvel.Treachery.Cards

allEncounterCards :: HashMap CardCode CardDef
allEncounterCards =
  fromList [(toCardCode def, def) | def <- HashMap.elems Allies.allAllies, isJust (cdEncounterSet def)] <>
  allAttachments <> allMinions <> allTreacheries <> allSideSchemes <> allSpecialMinions <> allObligations <> Environments.allEnvironments <> allEncounterSupports
    <> fromList [(toCardCode def,def) | def <- CW.civilWarDefinitions,("56" `Text.isPrefixOf` unCardCode (toCardCode def) || cdEncounterSet def `elem` [Just CwShieldOps,Just CwYoungAvengers,Just CwThunderboltsModular,Just CwTaskmasterModular]),cdCardType def `notElem` [VillainType,MainSchemeType]]

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

-- | A player's obligation, shuffled in with their identity, not one of an encounter set.
isPlayerObligation :: (HasCardDef a) => a -> Bool
isPlayerObligation (getCardDef -> def) = cdCardType def == ObligationType && isNothing (cdEncounterSet def)
