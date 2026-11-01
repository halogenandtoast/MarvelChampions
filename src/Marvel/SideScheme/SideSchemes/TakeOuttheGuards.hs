module Marvel.SideScheme.SideSchemes.TakeOuttheGuards where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Domino ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

takeOuttheGuards :: SideSchemeCard TakeOuttheGuards
takeOuttheGuards = sideScheme TakeOuttheGuards Cards.takeOuttheGuards (PerPlayer 4)
newtype TakeOuttheGuards = TakeOuttheGuards (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TakeOuttheGuards where
  runMessage msg a@(TakeOuttheGuards attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- selectList AnyIdentity
      for_ players $ cardSpecific attrs "discard"
      pure a
    CardSpecific (isTarget attrs -> True) "discard" (fromJSON -> Success you) -> do
      minions <- filterM (fmap (\ms -> CannotBeDiscarded `notElem` ms && CannotLeavePlay `notElem` ms) . getModifiers . toTarget) =<< selectList (NotMinion $ MinionWithTrait Trait.Elite)
      chooseOne you $ labeled "$label.noResponse" (pure ())
        : [targeting mid (removeFromPlay mid) | mid <- minions]
      pure a
    _ -> TakeOuttheGuards <$> runMessage msg attrs
