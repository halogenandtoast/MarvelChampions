module Marvel.Attachment.Attachments.PsionicIllusion where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

psionicIllusion :: AttachmentCard PsionicIllusion
psionicIllusion = attachment (PsionicIllusion . (`With` (Nothing :: Maybe (IdentityId, EnemyId, Resource, [EnemyId])))) Cards.psionicIllusion

newtype PsionicIllusion = PsionicIllusion (Attrs Attachment `With` Maybe (IdentityId, EnemyId, Resource, [EnemyId]))
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAttachment PsionicIllusion where
  toAttachmentAttrs (PsionicIllusion (attrs `With` _)) = attrs
instance IsRef PsionicIllusion where
  toRef = toRef . toAttachmentAttrs

instance HasAbilities PsionicIllusion where
  getAbilities a@(PsionicIllusion (attrs `With` _)) =
    [limitedWindowAbility a 1 (W.IdentityTargetsEnemy W.When (IdentityWithId you) AnyEnemy)
      ForcedInterrupt NoCriteria NoCost (runAbility a 1)
    | Just you <- [attachmentIdentity attrs]]

instance RunMessage PsionicIllusion where
  runMessage msg a@(PsionicIllusion (attrs `With` named)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> a <$ attachToIdentity aid you
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      cardSpecific attrs "checkTargets" (you, nub [enemy | W.IdentityAttackTarget ident enemy <- windows, ident == you])
      pure a
    CardSpecific (isTarget attrs -> True) "checkTargets" (fromJSON -> Success (you :: IdentityId, enemies :: [EnemyId])) -> do
      case enemies of
        [] -> pure ()
        enemy : remaining -> chooseOne you
          [labeled label (cardSpecific attrs "nameResource" (you, enemy, resource, remaining))
          | (label, resource) <- [("$label.mental", Mental), ("$label.physical", Physical), ("$label.energy", Energy), ("$label.wild", Wild)]]
      pure a
    CardSpecific (isTarget attrs -> True) "nameResource" (fromJSON -> Success choice@(you, _, _, _)) -> do
      discardFromDeck you 1 attrs
      pure $ PsionicIllusion (attrs `With` Just choice)
    WithDiscarded (isTarget attrs -> True) FromDeck cards | Just (you, enemy, resource, remaining) <- named -> do
      if any (elem resource . printedResources . getCardDef) cards
        then cardSpecific attrs "checkTargets" (you, remaining)
        else do
          characters <- selectList $ CharacterControlledBy AnyIdentity
          chooseOne you
            [targeting (characterRef character) do
              redirectCurrentPlayerAttack you enemy character
              removeFromPlay attrs
            | character <- characters]
      pure $ PsionicIllusion (attrs `With` Nothing)
    _ -> (\attrs' -> PsionicIllusion (attrs' `With` named)) <$> runMessage msg attrs
