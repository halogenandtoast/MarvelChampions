module Marvel.SideScheme.SideSchemes.TheMastersOfEvil
  ( theMastersOfEvil
  , TheMastersOfEvil(..)
  )
where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target
import Marvel.Trait

theMastersOfEvil :: SideSchemeCard TheMastersOfEvil
theMastersOfEvil = sideScheme TheMastersOfEvil Cards.theMastersOfEvil (PerPlayer 3)

newtype TheMastersOfEvil = TheMastersOfEvil SideSchemeAttrs
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage TheMastersOfEvil where
  runMessage msg ss@(TheMastersOfEvil attrs) = case msg of
    SideSchemeMessage sideSchemeId msg' | sideSchemeId == toId attrs ->
      case msg' of
        RevealSideScheme -> do
          ss <$ push (DiscardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait MastersOfEvil) (toTarget attrs))

        _ -> TheMastersOfEvil <$> runMessage msg attrs
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      firstPlayer <- selectJust FirstPlayer
      ss <$ push (RevealEncounterCard firstPlayer card)
    _ -> TheMastersOfEvil <$> runMessage msg attrs
