module Marvel.Ally.Allies.Sunfire where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Keyword ()
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

sunfire :: AllyCard Sunfire
sunfire = ally Sunfire Cards.sunfire (Thw 1, 1) (Atk 2, 1) (HP 2)

newtype Sunfire = Sunfire (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Sunfire where
  getAbilities (Sunfire a) =
    [limitedWindowAbility a 1 (W.PlayedFromHand (IdentityWithId $ allyController a) $ CardIs Cards.sunfire)
      Response (OwnsThis <> AttachmentExists AttachmentWithPrintedHeroActionOrResponse)
      (ResourceCost $ Just Energy) (runAbility a 1)]

instance RunMessage Sunfire where
  runMessage msg e@(Sunfire attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      attachments <- selectList AttachmentWithPrintedHeroActionOrResponse
      chooseOne you [targeting aid (removeFromPlay aid) | aid <- attachments]
      pure e
    _ -> Sunfire <$> runMessage msg attrs
