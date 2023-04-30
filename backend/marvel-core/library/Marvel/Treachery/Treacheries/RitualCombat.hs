module Marvel.Treachery.Treacheries.RitualCombat
  ( ritualCombat
  , RitualCombat(..)
  ) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

ritualCombat :: TreacheryCard RitualCombat
ritualCombat = treachery RitualCombat Cards.ritualCombat

newtype RitualCombat = RitualCombat (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage RitualCombat where
  runMessage msg t@(RitualCombat attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        push $ DiscardTopOfEncounterDeck 1 (Just $ toTarget attrs)
        pure . RitualCombat $ attrs & resolverL ?~ identityId
      _ -> RitualCombat <$> runMessage msg attrs
    WithDiscarded target _ (onlyEncounterCards -> cards)
      | isTarget attrs target -> do
        case treacheryResolver attrs of
          Just ident -> do
            let
              x = (+ 1) . fromIntegral . sum $ map
                (length . cdBoostIcons . getCardDef)
                cards
            chooseOne
              ident
              [ Label
                ("Deal " <> tshow x <> " damage to your hero")
                [ DamageCharacter
                    (IdentityCharacter ident)
                    (toSource attrs)
                    (toDamage x FromTreachery)
                ]
              , Label
                ("Place " <> tshow x <> " threat on the main scheme")
                [PlaceThreat (toSource attrs) x MainScheme]
              ]
            pure t
          Nothing -> error "no ident set"
    _ -> RitualCombat <$> runMessage msg attrs
