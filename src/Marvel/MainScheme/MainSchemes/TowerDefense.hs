-- | Tower Defense's main schemes (The Mad Titan's Shadow): Under Siege and The Armies of
-- Thanos, both in play at once, each a villain's own. Neither is ever completed.
module Marvel.MainScheme.MainSchemes.TowerDefense (
  underSiege,
  UnderSiege (..),
  theArmiesOfThanos,
  TheArmiesOfThanos (..),
) where

import Marvel.Prelude

import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Environment.Environments.AvengersTower (damageAvengersTower)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref

-- | "Forced Interrupt: When this stage would be completed, remove all the threat from
-- this stage instead. Then, …": once, for a second completion queued finds it cleared.
instead :: (ReverseQueue m) => m () -> Attrs MainScheme -> m (Attrs MainScheme)
instead body attrs = do
  threshold <- getThreshold attrs
  if mainSchemeThreat attrs >= threshold
    then (attrs & threatL .~ 0) <$ body
    else pure attrs

underSiege :: MainSchemeCard UnderSiege
underSiege =
  mainSchemeWith UnderSiege Cards.underSiege (PerPlayer 6) (PerPlayer 1) (PerPlayer 1) \a ->
    a {mainSchemeVillain = Just "Proxima Midnight"}

newtype UnderSiege = UnderSiege (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1A Setup: reveal stage 2A and put it into play next to this stage. Then the modular
-- difficulty (MTS rulebook p.11): the first player chooses the damage placed on Avengers
-- Tower, 1, 2 or 3 per player suggested for standard, expert and heroic play.
-- 1B, Proxima Midnight's Scheme: completed, its threat is removed instead, then 6[per
-- player] damage is dealt to Avengers Tower.
instance RunMessage UnderSiege where
  runMessage msg ms@(UnderSiege attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      addMainScheme (toCardCode Cards.theArmiesOfThanos)
      cardSpecific attrs "modularDifficulty" ()
      pure ms
    CardSpecific (isTarget attrs -> True) "modularDifficulty" _ -> do
      first <- selectJust FirstPlayer
      chooseOnePrompted first "$label.chooseTowerDefenseDifficulty"
        [ labeled (countLabel "towerDamagePerPlayer" n) (damageAvengersTower . fromIntegral =<< fromGameValue (PerPlayer $ fromIntegral n))
        | n <- [0 .. 3]
        ]
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) ->
      UnderSiege <$> instead (damageAvengersTower . fromIntegral =<< fromGameValue (PerPlayer 6)) attrs
    _ -> UnderSiege <$> runMessage msg attrs

theArmiesOfThanos :: MainSchemeCard TheArmiesOfThanos
theArmiesOfThanos =
  mainSchemeWith TheArmiesOfThanos Cards.theArmiesOfThanos (PerPlayer 6) (PerPlayer 1) (PerPlayer 1) \a ->
    a {mainSchemeVillain = Just "Corvus Glaive"}

newtype TheArmiesOfThanos = TheArmiesOfThanos (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 2A When Revealed: put the Avengers Tower environment into play, Stronghold side
-- faceup; put Focused Defense into play attached to this stage; each player searches the
-- encounter deck for a copy of Black Order Besieger and puts it into play engaged with
-- them; shuffle the encounter deck. 2B, Corvus Glaive's Scheme: completed, its threat is
-- removed instead, then each player is dealt 1 facedown encounter card.
instance RunMessage TheArmiesOfThanos where
  runMessage msg ms@(TheArmiesOfThanos attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      first <- selectJust FirstPlayer
      putEncounterCardIntoPlay first =<< genEncounterCard Environments.avengersTowerStronghold
      putEncounterCardIntoPlay first =<< genEncounterCard Attachments.focusedDefense
      players <- getPlayers
      for_ players \p -> searchAndPutIntoPlayEncounterCard p (CardIs Minions.blackOrderBesieger)
      shuffleEncounterDeck
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) ->
      TheArmiesOfThanos <$> instead (getPlayers >>= traverse_ dealEncounterCard) attrs
    _ -> TheArmiesOfThanos <$> runMessage msg attrs
