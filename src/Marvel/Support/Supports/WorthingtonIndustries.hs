module Marvel.Support.Supports.WorthingtonIndustries where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Trait qualified as Trait
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

worthingtonIndustries :: SupportCard WorthingtonIndustries
worthingtonIndustries = support WorthingtonIndustries Cards.worthingtonIndustries

newtype WorthingtonIndustries = WorthingtonIndustries (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WorthingtonIndustries where
  getAbilities a@(WorthingtonIndustries attrs) =
    [ability a 1 Action (OwnsThis <> AnyCriteria
      [ ExtendedCardExists (InDiscardOf (IdentityWithId $ supportController attrs) $ BasicCardMatches $ CardWithTrait Trait.Aerial)
      , InAlterEgoForm
      ]) ExhaustCost (runAbility a 1)]

instance RunMessage WorthingtonIndustries where
  runMessage msg a@(WorthingtonIndustries attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardWithTrait Trait.Aerial)
      if null cards
        then cardSpecific attrs "draw" you
        else chooseOne you [targeting c (shuffleIntoDeck you [c] >> cardSpecific attrs "draw" you) | c <- cards]
      pure a
    CardSpecific (isTarget attrs -> True) "draw" (fromJSON -> Success you) -> do
      whenM (identityMatches AlterEgoIdentity you) $ drawCards you 1
      pure a
    _ -> WorthingtonIndustries <$> runMessage msg attrs
