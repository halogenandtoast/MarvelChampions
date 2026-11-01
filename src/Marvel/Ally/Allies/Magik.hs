module Marvel.Ally.Allies.Magik where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

magik :: AllyCard Magik
magik = ally Magik Cards.magik (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Magik = Magik (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Magik where
  getAbilities (Magik a) =
    [limitedWindowAbility a 1 (W.PlayedFromHand (IdentityWithId $ allyController a) $ CardIs Cards.magik)
      Response (OwnsThis <> MinionExists (NotMinion (MinionWithTrait Trait.Elite) <> MinionEngagedWith (HeroIdentity <> IdentityWithTrait Trait.XMen)))
      (ResourceCost $ Just Mental) (runAbility a 1)]

instance RunMessage Magik where
  runMessage msg e@(Magik attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      minions <- selectList (NotMinion (MinionWithTrait Trait.Elite) <> MinionEngagedWith (HeroIdentity <> IdentityWithTrait Trait.XMen))
      unless (null minions) $ chooseOne you [targeting (MinionRef mid) (shuffleMinionIntoEncounterDeck mid) | mid <- minions]
      pure e
    _ -> Magik <$> runMessage msg attrs
