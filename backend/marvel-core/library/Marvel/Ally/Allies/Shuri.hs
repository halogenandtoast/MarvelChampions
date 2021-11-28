module Marvel.Ally.Allies.Shuri
  ( shuri
  , Shuri(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

shuri :: AllyCard Shuri
shuri = ally Shuri Cards.shuri (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Shuri = Shuri AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities Shuri where
  getAbilities (Shuri a) =
    [ limitedWindowAbility
        a
        1
        (PlayThis After)
        Response
        OwnsThis
        NoCost
        (RunAbility (toTarget a) 1)
    ]

instance RunMessage Shuri where
  runMessage msg a@(Shuri attrs) = case msg of
    RanAbility target 1 _ | isTarget attrs target -> do
      pushAll $ map
        (IdentityMessage (allyController attrs))
        [ SearchIdentityDeck (CardWithType UpgradeType) SearchDrawOne
        , ShuffleDeck
        ]
      pure a
    _ -> Shuri <$> runMessage msg attrs
