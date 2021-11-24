module Marvel.Ally.Allies.HawkeyeClintBarton where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import qualified Marvel.Ally.Cards as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

hawkeyeClintBarton :: AllyCard HawkeyeClintBarton
hawkeyeClintBarton = allyWith
  HawkeyeClintBarton
  Cards.hawkeyeClintBarton
  (Thw 1, 1)
  (Atk 1, 1)
  (HP 3)
  (countersL .~ 4)

newtype HawkeyeClintBarton = HawkeyeClintBarton AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities HawkeyeClintBarton where
  getAbilities (HawkeyeClintBarton a) =
    [ limitedWindowAbility
          a
          1
          (MinionEntersPlay When AnyMinion)
          Response
          OwnsThis
          UseCost
        $ RunAbility (toTarget a) 1
    ]

findMinion :: [WindowType] -> MinionId
findMinion = \case
  [] -> error "Invalid Call"
  MinionEnteredPlay minionId : _ -> minionId
  (_ : xs) -> findMinion xs

instance RunMessage HawkeyeClintBarton where
  runMessage msg a = case msg of
    RanAbility target 1 windows | isTarget a target -> do
      let minionId = findMinion windows
      push $ MinionMessage minionId (MinionDamaged (toSource a) 2)
      pure a
    _ -> HawkeyeClintBarton <$> runMessage msg (toAttrs a)
