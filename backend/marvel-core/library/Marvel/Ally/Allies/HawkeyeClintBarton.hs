module Marvel.Ally.Allies.HawkeyeClintBarton where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import qualified Marvel.Ally.Cards as Cards
import Marvel.Card.Code
import Marvel.Criteria
import Marvel.Cost
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
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
  (HP 1)
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

instance RunMessage HawkeyeClintBarton where
  runMessage msg a = HawkeyeClintBarton <$> runMessage msg (toAttrs a)
