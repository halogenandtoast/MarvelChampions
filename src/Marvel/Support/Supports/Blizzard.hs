module Marvel.Support.Supports.Blizzard where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Game.Source (asYou, weatherSpecialCanResolve)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Damage
import Marvel.GameValue
import Marvel.Effect.Types
import Marvel.Trait
import Marvel.Question
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

blizzard :: SupportCard Blizzard
blizzard = support Blizzard Cards.blizzard

newtype Blizzard = Blizzard (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Blizzard where
  getModifiersFor _ target _ = pure [AttackPenalty 1 | case target of
    IdentityRef _ -> True
    AllyRef _ -> True
    VillainRef _ -> True
    MinionRef _ -> True
    _ -> False]

instance RunMessage Blizzard where
  runMessage msg a@(Blizzard attrs) = runQueueT $ case msg of
    ResolveWeatherSpecial you sid | sid == supportId attrs -> do
      whenM (asYou you $ weatherSpecialCanResolve you sid) do
        minions <- selectList (NotMinion $ MinionWithTrait Elite)
        chooseOne you [targeting (MinionRef mid) $ createEffect Cards.blizzard attrs (MinionEntity $ MinionWithId mid) | mid <- minions]
        finishWeatherSpecial you sid
      pure a
    _ -> Blizzard <$> runMessage msg attrs

newtype BlizzardEffect = BlizzardEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

blizzardEffect :: CardEffect BlizzardEffect
blizzardEffect = effectWith BlizzardEffect Cards.blizzard ((modifiersL .~ [Blank]) . (endsL ?~ DisableAtEndOfRound))

instance RunMessage BlizzardEffect where
  runMessage msg (BlizzardEffect attrs) = BlizzardEffect <$> runMessage msg attrs
