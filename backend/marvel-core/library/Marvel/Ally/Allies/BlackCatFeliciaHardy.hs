module Marvel.Ally.Allies.BlackCatFeliciaHardy where

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
import Marvel.Resource
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

blackCatFeliciaHardy :: AllyCard BlackCatFeliciaHardy
blackCatFeliciaHardy = ally
  BlackCatFeliciaHardy
  Cards.blackCatFeliciaHardy
  (Thw 1, 1)
  (Atk 1, 0)
  (HP 2)

newtype BlackCatFeliciaHardy = BlackCatFeliciaHardy AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities BlackCatFeliciaHardy where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (PlayThis After)
        ForcedResponse
        OwnsThis
        NoCost
        (RunAbility (toTarget a) 1)
    ]

instance RunMessage BlackCatFeliciaHardy where
  runMessage msg a = case msg of
    RanAbility target 1 _ | isTarget a target -> do
      push $ IdentityMessage (allyController $ toAttrs a) $ DiscardFrom
        FromDeck
        2
        (Just target)
      pure a
    WithDiscarded target _ cards | isTarget a target -> do
      pushAll
        $ map (IdentityMessage (allyController $ toAttrs a) . AddToHand)
        $ filter (cardMatch $ CardWithResource Mental) cards
      pure a
    _ -> BlackCatFeliciaHardy <$> runMessage msg (toAttrs a)
