module Marvel.SideScheme.SideSchemes.SmSinisterSix where

import Marvel.Prelude
import Marvel.Ability.Types
import Data.Aeson (Result (..), fromJSON)
import Marvel.Villain.Villains.SmSinisterSix qualified as Six
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.GameValue
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newtype SmSinisterSixSideScheme = SmSinisterSixSideScheme (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smLightAtTheEndA :: SideSchemeCard SmSinisterSixSideScheme
smLightAtTheEndA = sideSchemeWith SmSinisterSixSideScheme Cards.smLightAtTheEndA (Static 10) (crisisL .~ False)

smLightAtTheEndB :: SideSchemeCard SmSinisterSixSideScheme
smLightAtTheEndB = sideSchemeWith SmSinisterSixSideScheme Cards.smLightAtTheEndB (Static 5) (crisisL .~ False)

smBruteForceBarricade :: SideSchemeCard SmSinisterSixSideScheme
smBruteForceBarricade = sideSchemeWith SmSinisterSixSideScheme Cards.smBruteForceBarricade (Static 9) (crisisL .~ False)

instance HasModifiersFor SmSinisterSixSideScheme where
  getModifiersFor _ (SideSchemeRef sid) (SmSinisterSixSideScheme a)
    | sid == sideSchemeId a, toCardCode a `elem` ["27102a", "27102b"] = pure [CannotLeavePlay]
    | toCardCode a == "27107", sid /= sideSchemeId a = pure [ThreatCannotBeRemoved]
  getModifiersFor _ _ _ = pure []

instance RunMessage SmSinisterSixSideScheme where
  runMessage msg e@(SmSinisterSixSideScheme attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27107" -> e <$ dealBoost enemy
    CardSpecific target "flipAfterAmbush" (fromJSON -> Success (_entered :: Maybe VillainId, _payload :: Value))
      | isTarget attrs target, toCardCode attrs == "27102a" -> do
          -- The owner ruling applies the Chase face's printed starting threat and
          -- hinder, preserving threat placed by the intervening Ambush.
          hinder <- fromIntegral <$> fromGameValue (PerPlayer 10)
          pure $ SmSinisterSixSideScheme attrs
            { sideSchemeCardDef = Cards.smLightAtTheEndB
            , sideSchemeInitialThreat = Static 5
            , sideSchemeThreat = sideSchemeThreat attrs + 5 + hinder
            }
    CardSpecific target "resolveThwart" (fromJSON -> Success (source, n :: Natural)) | target == toRef attrs -> do
      clearsScheme source n True
      SmSinisterSixSideScheme <$> runMessage msg attrs
    SideSchemeMessage sid (SideSchemeThreatRemoved source n) | sid == sideSchemeId attrs -> do
      clearsScheme source n False
      SmSinisterSixSideScheme <$> runMessage msg attrs
    _ -> SmSinisterSixSideScheme <$> runMessage msg attrs

   where
    clearsScheme source n thwarting = do
      modifiers <- getModifiers attrs
      controller <- getSourceController source
      let barred = ThreatCannotBeRemoved `elem` modifiers
            || (thwarting && ThreatCannotBeRemovedByThwarting `elem` modifiers)
            || any (\p -> maybe False (/= p) controller) [p | ThreatRemovableOnlyBy p <- modifiers]
          clears = sideSchemeThreat attrs > 0 && n >= sideSchemeThreat attrs && not barred
      when clears $ case toCardCode attrs of
        "27102a" -> forcedResponse attrs $ Six.ambushThen attrs "flipAfterAmbush" ()
        "27102b" -> forcedResponse attrs $ gameOver Won
        _ -> pure ()
