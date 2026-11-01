module Marvel.SideScheme.SideSchemes.Rampage (
  rampage,
  Rampage (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | An acceleration icon, its card's.
rampage :: SideSchemeCard Rampage
rampage = sideScheme (Rampage . (`With` Meta Nothing)) Cards.rampage (Static 4)

-- | The player whose thwart defeats it.
newtype Meta = Meta {defeatedBy :: Maybe IdentityId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Rampage = Rampage (Attrs SideScheme `With` Meta)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasCardCode Rampage where
  toCardCode (Rampage (a `With` _)) = toCardCode a

instance IsSideScheme Rampage where
  toSideSchemeAttrs (Rampage (a `With` _)) = a

-- | When Defeated: discard cards from the top of the encounter deck until a minion is
-- discarded, and put that minion into play engaged with the player who defeated this
-- scheme.
instance RunMessage Rampage where
  runMessage msg s@(Rampage (attrs `With` meta)) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      SideSchemeThwarted source _ -> do
        player <- maybe getActivePlayerId pure =<< getSourceController source
        attrs' <- runMessage msg attrs
        pure . Rampage $ attrs' `With` Meta (Just player)
      DefeatSideScheme -> do
        discardUntil FromEncounterDeck (CardWithType MinionType) attrs
        pure s
      _ -> Rampage . (`With` meta) <$> runMessage msg attrs
    WithDiscardedMatch target FromEncounterDeck (EncounterCard minion) | isTarget attrs target -> do
      player <- maybe getActivePlayerId pure (defeatedBy meta)
      putEncounterCardIntoPlay player minion
      pure s
    _ -> Rampage . (`With` meta) <$> runMessage msg attrs
