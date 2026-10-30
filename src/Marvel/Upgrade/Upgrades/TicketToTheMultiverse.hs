module Marvel.Upgrade.Upgrades.TicketToTheMultiverse where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hand
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

ticketToTheMultiverse :: UpgradeCard TicketToTheMultiverse
ticketToTheMultiverse = upgrade TicketToTheMultiverse Cards.ticketToTheMultiverse

newtype TicketToTheMultiverse = TicketToTheMultiverse (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TicketToTheMultiverse where
  getAbilities a =
    [ ability a 1 Action OwnsThis (RemoveFromGameCost $ toRef a) (runAbility a 1) ]

instance RunMessage TicketToTheMultiverse where
  runMessage msg u@(TicketToTheMultiverse attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      hand <- projectMap PlayerIdentityHand unHand you
      traverse_ discardCard hand
      shuffleDiscardIntoDeck you
      drawToHandSize you
      cardSpecific attrs "readyGhostSpiderCards" you
      pure u
    CardSpecific (isTarget attrs -> True) "readyGhostSpiderCards" (fromJSON -> Success you) -> do
      identities <- selectList (IdentityWithId you <> IdentityIs Heroes.ghostSpider)
      supports <- selectList (SupportMatches [SupportNamed "George Stacy", SupportControlledBy $ IdentityWithId you])
      upgrades <- selectList (UpgradeIs Cards.webBracelet <> upgradeControlledBy you)
      traverse_ ready identities
      traverse_ ready supports
      traverse_ ready upgrades
      pure u
    _ -> TicketToTheMultiverse <$> runMessage msg attrs
