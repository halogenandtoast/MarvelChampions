module Marvel.Minion.Minions.HydraBomber
  ( hydraBomber
  , HydraBomber(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Choice
import Marvel.Damage
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

hydraBomber :: MinionCard HydraBomber
hydraBomber = minion HydraBomber Cards.hydraBomber (Sch 1) (Atk 1) (HP 2)

newtype HydraBomber = HydraBomber (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage HydraBomber where
  runMessage msg e@(HydraBomber attrs) = case msg of
    MinionMessage ident msg' | ident == minionId attrs -> case msg' of
      RevealMinion identityId -> do
        chooseOne
          identityId
          [ Label
            "Take 2 damage"
            [ DamageCharacter
                (IdentityCharacter identityId)
                (toSource attrs)
                (toDamage 2 FromAbility)
            ]
          , Label
            "Place 1 threat on the main scheme"
            [PlaceThreat (toSource attrs) 1 MainScheme]
          ]
        pure e
      _ -> HydraBomber <$> runMessage msg attrs
    _ -> HydraBomber <$> runMessage msg attrs
