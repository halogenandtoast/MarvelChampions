module Marvel.Minion.Minions.Avalanche (
  avalanche,
  Avalanche (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Game.Source
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

avalanche :: MinionCard Avalanche
avalanche = minion Avalanche Cards.avalanche (Sch 1) (Atk 2) (HP 4)

newtype Avalanche = Avalanche (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Incite 2 (the engine places it as the card is revealed). When Revealed: each player
-- must choose to either take 2 indirect damage or exhaust their identity; an exhausted
-- identity cannot be chosen to exhaust (RR Choose (Option)).
instance RunMessage Avalanche where
  runMessage msg e@(Avalanche attrs) = runQueueT $ case msg of
    MinionMessage ident (RevealMinion _) | ident == minionId attrs -> do
      players <- getPlayers
      for_ players \p -> do
        canExhaust <- identityMatches UnexhaustedIdentity p
        chooseOrRunOne p $
          labeled "$label.take2IndirectDamage" (dealIndirectDamage p attrs 2)
            : [labeled "$label.exhaustYourIdentity" (exhaust p) | canExhaust]
      pure e
    _ -> Avalanche <$> runMessage msg attrs
