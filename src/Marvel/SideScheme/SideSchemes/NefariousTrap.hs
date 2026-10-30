module Marvel.SideScheme.SideSchemes.NefariousTrap where

import Marvel.Prelude
import Marvel.Card
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

nefariousTrap :: SideSchemeCard NefariousTrap
nefariousTrap = sideScheme NefariousTrap Cards.nefariousTrap (Static 3)

newtype NefariousTrap = NefariousTrap (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage NefariousTrap where
  runMessage msg (NefariousTrap attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      blanked <- isBlanked attrs
      unless blanked do
        you <- getDefeatedBy attrs
        selectOne (MinionIs Minions.theGreenGobbler) >>= \case
          Just gobbler -> enemyAttacks gobbler you
          Nothing -> searchAndPutIntoPlayEncounterCard you (CardIs Minions.theGreenGobbler)
      NefariousTrap <$> runMessage msg attrs
    _ -> NefariousTrap <$> runMessage msg attrs
