module Marvel.Minion.Minions.Whirlwind
  ( whirlwind
  , Whirlwind(..)
  ) where

import Marvel.Prelude

import Data.List qualified as L
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Window

whirlwind :: MinionCard Whirlwind
whirlwind = minion Whirlwind Cards.whirlwind (Sch 1) (Atk 2) (HP 6)

newtype Whirlwind = Whirlwind MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities Whirlwind where
  getAbilities (Whirlwind a) =
    [ windowAbility
          a
          1
          (EnemyAttacked When (EnemyWithId $ EnemyMinionId $ toId a) You)
          ForcedResponse
          NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage Whirlwind where
  runMessage msg e@(Whirlwind attrs) = case msg of
    RanAbility target 1 [EnemyAttack _ ident] | isTarget attrs target -> do
      otherPlayers <- L.delete ident <$> getPlayers
      for_ otherPlayers $ \other -> pushAll
        [ DeclareDefense other (toEnemyId attrs) AnyDefense
        , MinionMessage (toId attrs) MinionAttacked
        ]
      pure e
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        players <- getPlayers
        e <$ pushAll
          (map
            (\ident -> IdentityMessage
              ident
              (IdentityDamaged (toSource attrs) (toDamage 1 FromAbility))
            )
            players
          )
      _ -> Whirlwind <$> runMessage msg attrs
    _ -> Whirlwind <$> runMessage msg attrs
