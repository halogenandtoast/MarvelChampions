module Marvel.SideScheme.SideSchemes.KeepThemBusy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Window qualified as W ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

keepThemBusy :: SideSchemeCard KeepThemBusy
keepThemBusy = sideScheme KeepThemBusy Cards.keepThemBusy (PerPlayer 3)

newtype KeepThemBusy = KeepThemBusy (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage KeepThemBusy where
  runMessage msg a@(KeepThemBusy attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      n <- fromGameValue (PerPlayer 5)
      removeThreat you attrs (fromIntegral n) MainScheme
      pure a
    _ -> KeepThemBusy <$> runMessage msg attrs
