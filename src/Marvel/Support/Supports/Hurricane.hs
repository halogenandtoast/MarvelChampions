module Marvel.Support.Supports.Hurricane where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Game.Source (asYou, weatherSpecialCanResolve)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Damage ()
import Marvel.GameValue
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

hurricane :: SupportCard Hurricane
hurricane = support Hurricane Cards.hurricane

newtype Hurricane = Hurricane (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Hurricane where
  getModifiersFor _ target _ = pure [KeywordModifier (Retaliate 1) | case target of
    IdentityRef _ -> True
    AllyRef _ -> True
    VillainRef _ -> True
    MinionRef _ -> True
    _ -> False]

instance RunMessage Hurricane where
  runMessage msg a@(Hurricane attrs) = runQueueT $ case msg of
    ResolveWeatherSpecial you sid | sid == supportId attrs -> do
      whenM (asYou you $ weatherSpecialCanResolve you sid) do
        schemes <- filterM (canMoveSchemeThreatFrom attrs) =<< selectList (SchemeWithThreat $ GreaterThan $ Static 0)
        chooseOne you [targeting (schemeRef scheme) $ thwartSchemeAs you scheme attrs 2 | scheme <- schemes]
        finishWeatherSpecial you sid
      pure a
    _ -> Hurricane <$> runMessage msg attrs
