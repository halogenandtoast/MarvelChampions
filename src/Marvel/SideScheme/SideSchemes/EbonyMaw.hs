-- | The side schemes of Ebony Maw's set and the Black Order and Armies of Titan modular
-- sets (The Mad Titan's Shadow).
module Marvel.SideScheme.SideSchemes.EbonyMaw (
  reactorOverload,
  ReactorOverload (..),
  theBlackOrder,
  TheBlackOrder (..),
  landingCraft,
  LandingCraft (..),
) where

import Marvel.Prelude

import Marvel.Card
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
import Marvel.Trait

-- | 3 threat (not per player), a crisis icon.
reactorOverload :: SideSchemeCard ReactorOverload
reactorOverload = sideSchemeWith ReactorOverload Cards.reactorOverload (Static 3) (crisisL .~ True)

newtype ReactorOverload = ReactorOverload (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: each player must choose to either take 2 damage or place 2 threat
-- here.
instance RunMessage ReactorOverload where
  runMessage msg s@(ReactorOverload attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p ->
        chooseOne p
          [ labeled (countLabel "takeDamage" 2) (damageIdentity p attrs (toDamage 2 FromAbility))
          , labeled (countLabel "placeThreatOnReactorOverload" 2) (placeThreat ident 2)
          ]
      pure s
    _ -> ReactorOverload <$> runMessage msg attrs

-- | 2 per player, a hazard icon.
theBlackOrder :: SideSchemeCard TheBlackOrder
theBlackOrder = sideScheme TheBlackOrder Cards.theBlackOrder (PerPlayer 2)

newtype TheBlackOrder = TheBlackOrder (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | While a Black Order minion is in play, threat cannot be removed from this side
-- scheme.
instance HasModifiersFor TheBlackOrder where
  getModifiersFor _ (SideSchemeRef sid) (TheBlackOrder a)
    | sid == sideSchemeId a = do
        blackOrder <- inAnyArea $ selectAny (MinionWithTrait BlackOrder)
        pure [ThreatCannotBeRemoved | blackOrder]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheBlackOrder where
  runMessage msg (TheBlackOrder attrs) = TheBlackOrder <$> runMessage msg attrs

-- | 2 per player, an acceleration icon.
landingCraft :: SideSchemeCard LandingCraft
landingCraft = sideScheme LandingCraft Cards.landingCraft (PerPlayer 2)

newtype LandingCraft = LandingCraft (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: discard cards from the top of the encounter deck until a minion is
-- discarded, and put it into play engaged with the player who defeated this scheme.
instance RunMessage LandingCraft where
  runMessage msg s@(LandingCraft attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      discardUntilIntoPlay you (CardWithType MinionType)
      LandingCraft <$> runMessage msg attrs
    _ -> LandingCraft <$> runMessage msg attrs
