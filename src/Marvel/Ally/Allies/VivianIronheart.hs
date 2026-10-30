module Marvel.Ally.Allies.VivianIronheart where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Effect.Types
import Marvel.Keyword
import Marvel.Query
import Marvel.Trait

vivianIronheart :: AllyCard VivianIronheart
vivianIronheart = ally VivianIronheart Cards.vivianIronheart (Thw 2, 2) (Atk 1, 1) (HP 2)

newtype VivianIronheart = VivianIronheart (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities VivianIronheart where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis HeroResponse
    (OwnsThis <> AnyCriteria [AttachmentExists (AttachmentMatches []), MinionExists (NotMinion $ MinionWithTrait Elite), SideSchemeExists (NotSideScheme $ SideSchemeWithKeyword Permanent)])
    NoCost (runAbility a 1)]

instance RunMessage VivianIronheart where
  runMessage msg a@(VivianIronheart attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      attachments <- selectList (AttachmentMatches [])
      minions <- selectList (NotMinion $ MinionWithTrait Elite)
      schemes <- selectList (NotSideScheme $ SideSchemeWithKeyword Permanent)
      chooseOne you $
        [targeting attachment (createEffect Cards.vivianIronheart attrs $ AttachmentEntity $ AttachmentWithId attachment) | attachment <- attachments]
        <> [targeting minion (createEffect Cards.vivianIronheart attrs $ MinionEntity $ MinionWithId minion) | minion <- minions]
        <> [targeting scheme (createEffect Cards.vivianIronheart attrs $ SideSchemeEntity $ SideSchemeWithId scheme) | scheme <- schemes]
      pure a
    _ -> VivianIronheart <$> runMessage msg attrs

newtype VivianIronheartEffect = VivianIronheartEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

vivianIronheartEffect :: CardEffect VivianIronheartEffect
vivianIronheartEffect = effectWith VivianIronheartEffect Cards.vivianIronheart
  ((modifiersL .~ [Blank]) . (endsL ?~ DisableAtEndOfRound))

instance RunMessage VivianIronheartEffect where
  runMessage msg (VivianIronheartEffect attrs) = VivianIronheartEffect <$> runMessage msg attrs
