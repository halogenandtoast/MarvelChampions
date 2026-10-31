module Marvel.Support.Supports.ClearSkies where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Game.Source (asYou, weatherSpecialCanResolve)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Damage
import Marvel.GameValue
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

clearSkies :: SupportCard ClearSkies
clearSkies = support ClearSkies Cards.clearSkies

newtype ClearSkies = ClearSkies (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ClearSkies where
  getModifiersFor _ target _ = pure [KeywordModifier Stalwart | case target of
    IdentityRef _ -> True
    AllyRef _ -> True
    VillainRef _ -> True
    MinionRef _ -> True
    _ -> False]

instance RunMessage ClearSkies where
  runMessage msg a@(ClearSkies attrs) = runQueueT $ case msg of
    ResolveWeatherSpecial you sid | sid == supportId attrs -> do
      whenM (asYou you $ weatherSpecialCanResolve you sid) do
        drawCards you 1
        finishWeatherSpecial you sid
      pure a
    _ -> ClearSkies <$> runMessage msg attrs
