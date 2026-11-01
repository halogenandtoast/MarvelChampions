module Marvel.AlterEgo.AlterEgos.RemyLeBeau where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations

remyLeBeau :: AlterEgoCard RemyLeBeau
remyLeBeau = alterEgo RemyLeBeau Cards.remyLeBeau (HP $ Static 9) (HandSize 6) (Rec 3) [Obligations.guildBusiness]

newtype RemyLeBeau = RemyLeBeau (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RemyLeBeau where
  getAbilities a =
    [ label "$label.thiefExtraordinaire" $ subtype Ability.Thwart $
        ability a 1 Action (IsSelf <> AnyCriteria [SelfMatches ConfusedIdentity, SchemeExists SchemeYouCanThwart]) (ExhaustCost <> LookAtTopEncounterCardsAndDiscardOneCost 2) (runAbility a 1)
    ]

instance RunMessage RemyLeBeau where
  runMessage msg ae@(RemyLeBeau attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      countBoostIcons attrs you [card | EncounterCard card <- paymentEncounterDiscards payment]
      pure ae
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) counts -> do
      removeThreat you attrs (sum $ map snd counts) SchemeYouCanThwart
      pure ae
    _ -> RemyLeBeau <$> runMessage msg attrs
