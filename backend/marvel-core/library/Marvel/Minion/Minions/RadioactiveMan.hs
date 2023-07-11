module Marvel.Minion.Minions.RadioactiveMan (
  radioactiveMan,
  RadioactiveMan (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

radioactiveMan :: MinionCard RadioactiveMan
radioactiveMan =
  minion RadioactiveMan Cards.radioactiveMan (Sch 1) (Atk 1) (HP 7)

newtype RadioactiveMan = RadioactiveMan (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RadioactiveMan where
  getAbilities (RadioactiveMan a) =
    [ windowAbility
        a
        1
        (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) You)
        ForcedResponse
        NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage RadioactiveMan where
  runMessage msg e@(RadioactiveMan attrs) = case msg of
    RanAbility ident target 1 _ _ | isTarget attrs target -> do
      push $ IdentityMessage ident $ DiscardFrom RandomFromHand 1 Nothing
      pure e
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        ident <- getActivePlayerId
        e <$ push (IdentityMessage ident $ DiscardFrom RandomFromHand 1 Nothing)
      _ -> RadioactiveMan <$> runMessage msg attrs
    _ -> RadioactiveMan <$> runMessage msg attrs
