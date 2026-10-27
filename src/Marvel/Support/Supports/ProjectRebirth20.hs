module Marvel.Support.Supports.ProjectRebirth20 (
  projectRebirth20,
  ProjectRebirth20 (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Identity.Types (Field (..))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

projectRebirth20 :: SupportCard ProjectRebirth20
projectRebirth20 = support ProjectRebirth20 Cards.projectRebirth20

newtype ProjectRebirth20 = ProjectRebirth20 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-Ego Action: exhaust → choose to either draw 1 card or heal 3 damage from Flash
-- Thompson (the controller's identity, in alter-ego form to use it).
instance HasAbilities ProjectRebirth20 where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost $ runAbility a 1]

-- | Healing an undamaged identity cannot be resolved, so it is not offered (RR Choose
-- (Option)).
instance RunMessage ProjectRebirth20 where
  runMessage msg s@(ProjectRebirth20 attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      damaged <- projectP PlayerIdentityDamage (> 0) ident
      chooseOne ident $
        labeled (countLabel "drawCards" 1) (drawCards ident 1)
          : [labeled (countLabel "healYourIdentity" 3) (heal ident 3) | damaged]
      pure s
    _ -> ProjectRebirth20 <$> runMessage msg attrs
