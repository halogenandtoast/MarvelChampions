module Marvel.Minion.Minions.RadioactiveMan
  ( radioactiveMan
  , RadioactiveMan(..)
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
import Marvel.Minion.Attrs
import qualified Marvel.Minion.Cards as Cards
import Marvel.Window

radioactiveMan :: MinionCard RadioactiveMan
radioactiveMan =
  minion RadioactiveMan Cards.radioactiveMan (Sch 1) (Atk 1) (HP 7)

newtype RadioactiveMan = RadioactiveMan MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities RadioactiveMan where
  getAbilities (RadioactiveMan a) =
    [ windowAbility
          a
          1
          (EnemyAttacked (EnemyWithId $ EnemyMinionId $ toId a) You)
          ForcedResponse
          NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage RadioactiveMan where
  runMessage msg e@(RadioactiveMan attrs) = case msg of
    RanAbility target 1 [EnemyAttack _ ident] | isTarget attrs target -> do
      e <$ push (IdentityMessage ident $ DiscardFrom RandomFromHand 1 Nothing)
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        ident <- getActivePlayerId
        e <$ push (IdentityMessage ident $ DiscardFrom RandomFromHand 1 Nothing)
      _ -> RadioactiveMan <$> runMessage msg attrs
    _ -> RadioactiveMan <$> runMessage msg attrs
