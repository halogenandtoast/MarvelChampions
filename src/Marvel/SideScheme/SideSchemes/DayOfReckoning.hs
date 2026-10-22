module Marvel.SideScheme.SideSchemes.DayOfReckoning (
  dayOfReckoning,
  DayOfReckoning (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Status

dayOfReckoning :: SideSchemeCard DayOfReckoning
dayOfReckoning = sideScheme DayOfReckoning Cards.dayOfReckoning (Static 6)

newtype DayOfReckoning = DayOfReckoning (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Wrecker's side scheme: it cannot leave play while he is in play, and leaves the game
-- with him (his signature scheme, set up by Breakout).
--
-- Hard Hitter — Forced Response: after threat is placed here, if there is 10 or more threat here,
-- deal 2 damage to each friendly character. Remove all but 3 threat from this scheme.
instance RunMessage DayOfReckoning where
  runMessage msg s@(DayOfReckoning attrs) = runQueueT $ case msg of
    SideSchemeMessage ident (SideSchemePlaceThreat n) | ident == sideSchemeId attrs -> do
      DayOfReckoning attrs' <- DayOfReckoning <$> runMessage msg attrs
      when (n > 0 && sideSchemeThreat attrs' >= 10) do
        damageAllCharacters attrs (toDamage 2 FromAbility) (CharacterControlledBy AnyIdentity)
        cardSpecific attrs "allButThree" ()
      pure (DayOfReckoning attrs')
    CardSpecific (isTarget attrs -> True) "allButThree" _ -> do
      removeSideSchemeThreat (sideSchemeId attrs) (subtractNatural 3 (sideSchemeThreat attrs))
      pure s
    _ -> DayOfReckoning <$> runMessage msg attrs
