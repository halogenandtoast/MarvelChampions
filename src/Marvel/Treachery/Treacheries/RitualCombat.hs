module Marvel.Treachery.Treacheries.RitualCombat (
  ritualCombat,
  RitualCombat (..),
) where

import Marvel.Prelude

import Marvel.Boost (boostCount)
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

ritualCombat :: TreacheryCard RitualCombat
ritualCombat = treachery RitualCombat Cards.ritualCombat

newtype RitualCombat = RitualCombat (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage RitualCombat where
  runMessage msg t@(RitualCombat attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        discardTopOfEncounterDeck 1 attrs
        pure . RitualCombat $ attrs & resolverL ?~ identityId
      _ -> RitualCombat <$> runMessage msg attrs
    WithDiscarded target _ (onlyEncounterCards -> cards)
      | isTarget attrs target -> do
          case treacheryResolver attrs of
            Just ident -> do
              let
                x =
                  -- a boost star is not a boost icon
                  (+ 1) . sum $
                    map
                      (boostCount . cdBoostIcons . getCardDef)
                      cards
              chooseOne
                ident
                [ labeled ("Deal " <> tshow x <> " damage to your hero") $
                    damageCharacter ident attrs (toDamage x FromTreachery)
                , labeled ("Place " <> tshow x <> " threat on the main scheme") $
                    placeThreatOnEach attrs x MainScheme
                ]
              pure t
            Nothing -> error "no ident set"
    _ -> RitualCombat <$> runMessage msg attrs
