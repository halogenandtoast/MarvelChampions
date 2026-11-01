module Marvel.Minion.Minions.Butler where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Stats ()

butler :: MinionCard Butler
butler = minion Butler Cards.butler (Sch 2) (Atk 1) (HP 3)

newtype Butler = Butler (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Butler where
  getModifiersFor _ target (Butler attrs)
    | isTarget attrs target = do
        scheme <- selectOne (SideSchemeIs Schemes.involuntaryProcedures)
        pure [MinionSchemesOn sid | Just sid <- [scheme]]
  getModifiersFor _ _ _ = pure []

instance RunMessage Butler where
  runMessage msg m@(Butler attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      confuseIdentity you
      pure m
    _ -> Butler <$> runMessage msg attrs
