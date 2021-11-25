module Marvel.Ally.Allies.MariaHill where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

mariaHill :: AllyCard MariaHill
mariaHill = ally MariaHill Cards.mariaHill (Thw 2, 1) (Atk 1, 1) (HP 2)

instance HasAbilities MariaHill where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (PlayThis After)
        Response
        OwnsThis
        NoCost
        (RunAbility (toTarget a) 1)
    ]

newtype MariaHill = MariaHill AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage MariaHill where
  runMessage msg a = case msg of
    RanAbility target 1 _ | isTarget a target -> do
      players <- getPlayers
      pushAll $ map (\p -> IdentityMessage p $ DrawCards FromDeck 1) players
      pure a
    _ -> MariaHill <$> runMessage msg (toAttrs a)
