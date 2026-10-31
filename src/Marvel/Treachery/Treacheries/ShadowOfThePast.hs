module Marvel.Treachery.Treacheries.ShadowOfThePast (
  shadowOfThePast,
  ShadowOfThePast (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

shadowOfThePast :: TreacheryCard ShadowOfThePast
shadowOfThePast = treachery ShadowOfThePast Cards.shadowOfThePast

newtype ShadowOfThePast = ShadowOfThePast (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | The nemesis minion, the side scheme, and the rest. A set with more than one minion
-- names its nemesis minion in parentheses (RR "Nemesis Encounter Set"): the others are
-- the rest.
partitionNemesisSet ::
  [EncounterCard] -> ([EncounterCard], [EncounterCard], [EncounterCard])
partitionNemesisSet cards = (minions, sideSchemes, rest)
 where
  isType t = (== t) . cdCardType . ecCardDef
  designated = Minions.designatedNemesisMinions
  isNemesisMinion c =
    isType MinionType c
      && ( toCardCode c `elem` map toCardCode designated
            || cdEncounterSet (ecCardDef c) `notElem` map cdEncounterSet designated
         )
  minions = filter isNemesisMinion cards
  sideSchemes = filter (isType SideSchemeType) cards
  rest = filter (\c -> not (isNemesisMinion c || isType SideSchemeType c)) cards

instance RunMessage ShadowOfThePast where
  runMessage msg t@(ShadowOfThePast attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      cards <- selectList (NemesisSetFor identityId)
      let (minions, _, _) = partitionNemesisSet cards
      for_ minions (revealEncounterCard identityId)
      -- A revealed nemesis may itself reveal another card from this set. Inspect
      -- what is still set aside only after those reveal effects have resolved.
      cardSpecific attrs "revealRemainingNemesis" identityId
      -- the nemesis minion did not enter play: it already has
      pure $ if null minions then ShadowOfThePast (attrs & surgeL .~ True) else t
    CardSpecific (isTarget attrs -> True) "revealRemainingNemesis" (fromJSON -> Success identityId) -> do
      cards <- selectList (NemesisSetFor identityId)
      let (_, sideSchemes, rest) = partitionNemesisSet cards
      for_ sideSchemes (revealEncounterCard identityId)
      when (notNull rest) $ shuffleIntoEncounterDeck rest
      pure t
    _ -> ShadowOfThePast <$> runMessage msg attrs
