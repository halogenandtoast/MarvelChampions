module Marvel.Support.Supports.Thunderstorm where

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

thunderstorm :: SupportCard Thunderstorm
thunderstorm = support Thunderstorm Cards.thunderstorm

newtype Thunderstorm = Thunderstorm (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Thunderstorm where
  getModifiersFor _ target _ = pure [AttackModifier 1 | case target of
    IdentityRef _ -> True
    AllyRef _ -> True
    VillainRef _ -> True
    MinionRef _ -> True
    _ -> False]

instance RunMessage Thunderstorm where
  runMessage msg a@(Thunderstorm attrs) = runQueueT $ case msg of
    ResolveWeatherSpecial you sid | sid == supportId attrs -> do
      whenM (asYou you $ weatherSpecialCanResolve you sid) do
        chooseDamage you attrs (toDamage 2 FromAbility) DamageableEnemy
        finishWeatherSpecial you sid
      pure a
    _ -> Thunderstorm <$> runMessage msg attrs
