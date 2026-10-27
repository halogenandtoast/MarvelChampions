module Marvel.Treachery.Treacheries.FanTheFlames (
  fanTheFlames,
  FanTheFlames (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

fanTheFlames :: TreacheryCard FanTheFlames
fanTheFlames = treachery FanTheFlames Cards.fanTheFlames

newtype FanTheFlames = FanTheFlames (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: take 2 indirect damage, 1 more if Blazing Inferno is in play and 1
-- more if Furnax is.
instance RunMessage FanTheFlames where
  runMessage msg t@(FanTheFlames attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      inferno <- selectAny (SideSchemeIs SideSchemes.blazingInferno)
      furnax <- selectAny (MinionIs Minions.furnax)
      dealIndirectDamage you attrs (2 + (if inferno then 1 else 0) + (if furnax then 1 else 0))
      pure t
    _ -> FanTheFlames <$> runMessage msg attrs
