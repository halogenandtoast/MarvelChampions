{-# LANGUAGE UndecidableInstances #-}
module Marvel.Ally.Allies.MariaHill where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import qualified Marvel.Ally.Cards as Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

mariaHill :: AllyCard MariaHill
mariaHill = ally MariaHill Cards.mariaHill (Thw 2) (Atk 1)

instance HasAbilities MariaHill where
  getAbilities a =
    [windowAbility a 1 (PlayThis After) Response (RunAbility (toTarget a) 1)]

newtype MariaHill = MariaHill AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage MariaHill where
  runMessage msg a = case msg of
    RanAbility target 1 | isTarget a target -> do
      players <- getPlayers
      pushAll $ map (\p -> IdentityMessage p $ DrawCards FromDeck 1) players
      pure a
    _ -> pure a
