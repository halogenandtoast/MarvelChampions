module Marvel.Minion.Minions.HydraBomber
  ( hydraBomber
  , HydraBomber(..)
  ) where

import Marvel.Prelude

import Marvel.Matchers
import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards

hydraBomber :: MinionCard HydraBomber
hydraBomber = minion HydraBomber Cards.hydraBomber (Sch 1) (Atk 1) (HP 2)

newtype HydraBomber = HydraBomber MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage HydraBomber where
  runMessage msg e@(HydraBomber attrs) = case msg of
    MinionMessage minionId msg' | minionId == toId attrs -> case msg' of
      RevealMinion ident -> do
        chooseOne
          ident
          [ Label
            "Take 2 damage"
            [DamageCharacter (IdentityCharacter ident) (toSource attrs) 2]
          , Label
            "Place 1 threat on the main scheme"
            [PlaceThreat (toSource attrs) 1 MainScheme]
          ]
        pure e
      _ -> HydraBomber <$> runMessage msg attrs
    _ -> HydraBomber <$> runMessage msg attrs
