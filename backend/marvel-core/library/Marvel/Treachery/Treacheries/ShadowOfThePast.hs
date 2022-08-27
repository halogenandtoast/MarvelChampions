module Marvel.Treachery.Treacheries.ShadowOfThePast
  ( shadowOfThePast
  , ShadowOfThePast(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

shadowOfThePast :: TreacheryCard ShadowOfThePast
shadowOfThePast = treachery ShadowOfThePast Cards.shadowOfThePast

newtype ShadowOfThePast = ShadowOfThePast (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

partitionNemesisSet
  :: [EncounterCard] -> ([EncounterCard], [EncounterCard], [EncounterCard])
partitionNemesisSet cards = (minions, sideSchemes, rest)
 where
  minions = filter ((== MinionType) . cdCardType . ecCardDef) cards
  sideSchemes = filter ((== SideSchemeType) . cdCardType . ecCardDef) cards
  rest = filter
    (and
    . sequence [(/= MinionType), (/= SideSchemeType)]
    . cdCardType
    . ecCardDef
    )
    cards

instance RunMessage ShadowOfThePast where
  runMessage msg t@(ShadowOfThePast attrs) = case msg of
    TreacheryMessage ident msg' | ident == treacheryId attrs -> case msg' of
      RevealTreachery identityId -> do
        cards <- selectList (NemesisSetFor identityId)
        let (minions, sideSchemes, rest) = partitionNemesisSet cards
        pushAll
          $ map (RevealEncounterCard identityId) minions
          <> map (RevealEncounterCard identityId) sideSchemes
          <> [ShuffleIntoEncounterDeck rest]
        pure t
      _ -> ShadowOfThePast <$> runMessage msg attrs
    _ -> ShadowOfThePast <$> runMessage msg attrs
