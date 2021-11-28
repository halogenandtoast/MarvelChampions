module Marvel.AlterEgo.AlterEgos.TChalla where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Attrs
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.Card.Def
import Marvel.Card.PlayerCard
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Trait

tChalla :: AlterEgoCard TChalla
tChalla = alterEgo
  TChalla
  Cards.tChalla
  (HP $ Static 11)
  (HandSize 6)
  (Rec 4)
  [Cards.affairsOfState]

newtype TChalla = TChalla AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, Entity)

instance HasAbilities TChalla where
  getAbilities (TChalla a) =
    [ability a 1 Setup NoCriteria NoCost $ RunAbility (toTarget a) 1]

instance RunMessage TChalla where
  runMessage msg a@(TChalla attrs) = case msg of
    RanAbility target 1 _ | isTarget attrs target -> do
      pushAll $ map
        (IdentityMessage (toId attrs))
        [ SearchIdentityDeck
          (CardWithTrait BlackPanther <> CardWithType UpgradeType)
          (toTarget attrs)
        , ShuffleDeck
        ]
      pure a
    SearchFoundCards target cards | isTarget attrs target -> do
      chooseOne
        (toId attrs)
        [ TargetLabel
            (CardIdTarget $ pcCardId c)
            [Run [IdentityMessage (toId attrs) $ AddToHand c]]
        | c <- cards
        ]
      pure a
    _ -> TChalla <$> runMessage msg attrs
