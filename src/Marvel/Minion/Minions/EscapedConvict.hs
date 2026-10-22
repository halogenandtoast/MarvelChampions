module Marvel.Minion.Minions.EscapedConvict (
  escapedConvict,
  EscapedConvict (..),
) where

import Marvel.Prelude

import Data.Aeson (Result (..), fromJSON)
import Marvel.Ability
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | Surge (printed).
escapedConvict :: MinionCard EscapedConvict
escapedConvict = minion EscapedConvict Cards.escapedConvict (Sch 1) (Atk 1) (HP 2)

newtype EscapedConvict = EscapedConvict (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Boost: move the active counter to the villain whose side scheme has the least
-- threat. If you are in hero form, that villain attacks you after this attack, the attack
-- getting no boost card.
instance RunMessage EscapedConvict where
  runMessage msg e@(EscapedConvict attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      withVillainBySideSchemeThreat LeastThreat \v -> do
        setActiveVillain v
        cardSpecific attrs "attacksAfter" (v, enemy)
      pure e
    Boost (CardSpecific (isTarget attrs -> True) "attacksAfter" (fromJSON -> Success (v, enemy))) -> do
      you <- getBoostedPlayer enemy
      isHero <- identityMatches HeroIdentity you
      when isHero $ activationAfter enemy v (villainAttacksWithoutBoost v you)
      pure e
    _ -> EscapedConvict <$> runMessage msg attrs
