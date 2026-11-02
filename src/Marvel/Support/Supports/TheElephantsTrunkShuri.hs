module Marvel.Support.Supports.TheElephantsTrunkShuri where
import Marvel.Prelude
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Discard ()
import Marvel.Deck ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Support.Types

theElephantsTrunkShuri :: SupportCard TheElephantsTrunkShuri
theElephantsTrunkShuri = support TheElephantsTrunkShuri Cards.theElephantsTrunkShuri
newtype TheElephantsTrunkShuri = TheElephantsTrunkShuri (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage TheElephantsTrunkShuri where
  runMessage msg u@(TheElephantsTrunkShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cardSpecific attrs "exhaust" (you,0 :: Natural)
      pure u
    CardSpecific (isTarget attrs -> True) "exhaust" (fromJSON -> Success (you,n :: Natural)) -> do
      allies <- selectList (AllyControlledBy (IdentityWithId you) <> AllyWithTrait T.Wakanda <> UnexhaustedAlly)
      supports <- selectList (SupportMatches [SupportControlledBy (IdentityWithId you), SupportWithTrait T.Wakanda, UnexhaustedSupport])
      let next = cardSpecific attrs "exhaust" (you,n+1)
      if n >= 2 then drawCards you 3 else chooseOne you (labeled "$label.done" (drawCards you (n+1)) : [targeting aid (exhaust aid >> next) | aid <- allies] <> [targeting sid (exhaust sid >> next) | sid <- supports])
      pure u
    _ -> TheElephantsTrunkShuri <$> runMessage msg attrs

instance HasAbilities TheElephantsTrunkShuri where
  getAbilities u@(TheElephantsTrunkShuri _attrs) = [ability u 1 AlterEgoAction OwnsThis ExhaustCost (runAbility u 1)]
