module Marvel.AlterEgo.AlterEgos.WadeWilson where

import Marvel.Prelude
import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Event.Cards qualified as Events
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats

wadeWilson :: AlterEgoCard WadeWilson
wadeWilson = alterEgo WadeWilson Cards.wadeWilson (HP $ Static 9) (HandSize 6) (Rec 8) [Obligations.theMercWithTheMouth]

newtype WadeWilson = WadeWilson (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

deadpoolEvent :: CardMatcher
deadpoolEvent = CardMatchesAny $ map CardIs
  [ Events.exhaustingPersonality, Events.maximumEffort, Events.metaknowledge
  , Events.yooHoo, Events.thisCardIsFire
  ]

instance HasAbilities WadeWilson where
  getAbilities a =
    [ label "$label.breakTheFourthWall" $
        limitedAbility a 1 (PerRound 1) Action IsSelf (DiscardHandCardsCost 1 $ Just 1) (runAbility a 1)
    ]

instance RunMessage WadeWilson where
  runMessage msg a@(WadeWilson attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeck you AllOfDeck) deadpoolEvent SearchDrawOne ShuffleBackIn
      pure a
    _ -> WadeWilson <$> runMessage msg attrs
