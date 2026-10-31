module Marvel.Support.Supports.TheThievesGuild where

import Marvel.Prelude
import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as AlterEgos
import Marvel.Card
import Marvel.Cost
import Marvel.Count
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

theThievesGuild :: SupportCard TheThievesGuild
theThievesGuild = support TheThievesGuild Cards.theThievesGuild

newtype TheThievesGuild = TheThievesGuild (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheThievesGuild where
  getAbilities a@(TheThievesGuild attrs) =
    [limitedWindowAbility a 1
      (W.ResolvesAbility W.After (IdentityWithId $ supportController attrs) (CardIs AlterEgos.remyLeBeau))
      AlterEgoResponse (OwnsThis <> SchemeExists ThwartableScheme) ExhaustCost (runAbility a 1)]

instance RunMessage TheThievesGuild where
  runMessage msg s@(TheThievesGuild attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      schemes <- selectList ThwartableScheme
      chooseOne you [targeting (schemeRef scheme) do
        before <- selectCount SchemeThreat (SchemeWithId scheme)
        thwartScheme scheme attrs 1
        cardSpecific attrs "guildThreat" (you, scheme, before)
        | scheme <- schemes]
      pure s
    CardSpecific (isTarget attrs -> True) "guildThreat" (fromJSON -> Success (you, scheme, before :: Natural)) -> do
      after <- selectCount SchemeThreat (SchemeWithId scheme)
      when (before > 0 && after == 0) $ drawCards you 1
      pure s
    _ -> TheThievesGuild <$> runMessage msg attrs
