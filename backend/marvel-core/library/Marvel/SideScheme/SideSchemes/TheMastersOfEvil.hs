module Marvel.SideScheme.SideSchemes.TheMastersOfEvil (
  theMastersOfEvil,
  TheMastersOfEvil (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

theMastersOfEvil :: SideSchemeCard TheMastersOfEvil
theMastersOfEvil =
  sideScheme TheMastersOfEvil Cards.theMastersOfEvil (PerPlayer 3)

newtype TheMastersOfEvil = TheMastersOfEvil (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TheMastersOfEvil where
  runMessage msg ss@(TheMastersOfEvil attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        push $
          DiscardUntil
            FromEncounterDeck
            (CardWithType MinionType <> CardWithTrait MastersOfEvil)
            (toTarget attrs)
        pure ss
      _ -> TheMastersOfEvil <$> runMessage msg attrs
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      firstPlayer <- selectJust FirstPlayer
      ss <$ push (RevealEncounterCard firstPlayer card)
    _ -> TheMastersOfEvil <$> runMessage msg attrs
