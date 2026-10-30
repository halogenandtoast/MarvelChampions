module Marvel.AlterEgo.AlterEgos.GwenStacy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Projection
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Upgrades

gwenStacy :: AlterEgoCard GwenStacy
gwenStacy = alterEgo GwenStacy Cards.gwenStacy (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.worriedFather]

newtype GwenStacy = GwenStacy (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities GwenStacy where
  getAbilities a =
    [ limitedAbility a 1 (PerRound 1) Action
        (IsSelf <> AnyCriteria
          [ ExtendedCardExists (InDiscardOf You $ BasicCardMatches $ CardIs Upgrades.ticketToTheMultiverse)
          , SupportExists (SupportNamed "George Stacy")
              <> NotCriteria (SupportExists $ SupportMatches [SupportNamed "George Stacy", UnexhaustedSupport])
          ]) NoCost (runAbility a 1)
    ]

instance RunMessage GwenStacy where
  runMessage msg ae@(GwenStacy attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      tickets <- filter (cardMatch $ CardIs Upgrades.ticketToTheMultiverse) <$> projectMap PlayerIdentityDiscard unDiscard you
      georges <- selectList (SupportNamed "George Stacy")
      readyGeorges <- selectList (SupportMatches [SupportNamed "George Stacy", UnexhaustedSupport])
      let choices = [targeting c (shuffleIntoDeck you [c]) | c <- tickets]
            <> [targeting sid (ready sid) | sid <- georges, sid `notElem` readyGeorges]
      unless (null choices) $ chooseOne you choices
      pure ae
    _ -> GwenStacy <$> runMessage msg attrs
