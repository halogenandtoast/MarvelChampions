module Marvel.Ally.Allies.MockingbirdBobbiMorse where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

mockingbirdBobbiMorse :: AllyCard MockingbirdBobbiMorse
mockingbirdBobbiMorse = ally
  MockingbirdBobbiMorse
  Cards.mockingbirdBobbiMorse
  (Thw 1, 1)
  (Atk 1, 1)
  (HP 3)

instance HasAbilities MockingbirdBobbiMorse where
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

newtype MockingbirdBobbiMorse = MockingbirdBobbiMorse AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage MockingbirdBobbiMorse where
  runMessage msg a = case msg of
    RanAbility target 1 _ | isTarget a target -> do
      enemies <- selectList AnyEnemy
      push $ Ask (allyController $ toAttrs a) $ ChooseOne $ map
        (stunChoice (toAttrs a))
        enemies
      pure a
    _ -> MockingbirdBobbiMorse <$> runMessage msg (toAttrs a)
