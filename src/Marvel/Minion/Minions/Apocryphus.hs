module Marvel.Minion.Minions.Apocryphus (
  apocryphus,
  Apocryphus (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

apocryphus :: MinionCard Apocryphus
apocryphus = minion Apocryphus Cards.apocryphus (Sch 1) (Atk 3) (HP 4)

newtype Apocryphus = Apocryphus (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Apocryphus where
  runMessage msg e@(Apocryphus attrs) = runQueueT $ case msg of
    -- When Revealed: discard an ally or support you control
    MinionMessage ident (RevealMinion you) | ident == minionId attrs -> do
      allies <- selectRefs $ AllyControlledBy (IdentityWithId you)
      supports <- selectRefs $ SupportControlledBy (IdentityWithId you)
      unless (null $ allies <> supports) $
        chooseOne you [targeting c (removeFromPlay c) | c <- allies <> supports]
      pure e
    -- [star] Boost: exhaust a character you control, and this enemy is given another
    -- boost card
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      identity <- selectList $ IdentityWithId you <> UnexhaustedIdentity
      allies <- selectList $ AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly
      unless (null identity && null allies) $
        chooseOne you $
          [targeting i (exhaust i) | i <- identity] <> [targeting a (exhaust a) | a <- allies]
      dealBoost enemy
      pure e
    _ -> Apocryphus <$> runMessage msg attrs
