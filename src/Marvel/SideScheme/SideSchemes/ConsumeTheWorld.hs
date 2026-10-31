module Marvel.SideScheme.SideSchemes.ConsumeTheWorld where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

consumeTheWorld :: SideSchemeCard ConsumeTheWorld
consumeTheWorld = sideSchemeWith ConsumeTheWorld Cards.consumeTheWorld (Static 6) updateAmplify

newtype ConsumeTheWorld = ConsumeTheWorld (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ConsumeTheWorld where
  getModifiersFor _ target (ConsumeTheWorld attrs) =
    pure [CannotLeavePlay | isTarget attrs target]

-- The card's constant ability changes its icon with its current threat, including
-- threat removed through the ordinary thwart pipeline.
updateAmplify :: Attrs SideScheme -> Attrs SideScheme
updateAmplify attrs = attrs
  { sideSchemeCardDef = (sideSchemeCardDef attrs) {cdAmplify = if sideSchemeThreat attrs == 0 then 0 else 1}
  }

instance RunMessage ConsumeTheWorld where
  runMessage msg (ConsumeTheWorld attrs) = runQueueT do
    attrs' <- runMessage msg attrs
    case msg of
      SideSchemeMessage sid (SideSchemePlaceThreat n) | sid == sideSchemeId attrs, n > 0 ->
        when (sideSchemeThreat attrs' >= 12) $ gameOver Lost
      SideSchemeMessage sid SideSchemePlaceInitialThreat | sid == sideSchemeId attrs ->
        when (sideSchemeThreat attrs' >= 12) $ gameOver Lost
      _ -> pure ()
    pure $ ConsumeTheWorld $ updateAmplify attrs'
