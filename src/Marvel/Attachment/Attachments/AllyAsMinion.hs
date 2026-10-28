-- | What Fallen Warrior (Legions of Hel) and Beguiled (Enchantress) share: each treats
-- the ally it is attached to as a minion with a blank text box ('allyAsMinion' in
-- "Marvel.Game"), its traits and the one the card gives.
module Marvel.Attachment.Attachments.AllyAsMinion (
  attachedAllyMinion,
  allyMinionTrait,
  revertsToAlly,
) where

import Marvel.Prelude

import Marvel.Attachment.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

-- | The minion it is attached to.
attachedAllyMinion :: Attrs Attachment -> Maybe MinionId
attachedAllyMinion a = case attachmentEnemy a of
  Just (EnemyMinionId m) -> Just m
  _ -> Nothing

-- | "Treat attached ally as an <trait> minion": the trait it gains, besides its printed
-- ones (RR Traits: they are not part of the text box).
allyMinionTrait :: (Applicative m) => Trait -> Attrs Attachment -> Target -> m [Modifier]
allyMinionTrait t a (MinionRef m) | attachedAllyMinion a == Just m = pure [TraitModifier t]
allyMinionTrait _ _ _ = pure []

-- | The card leaving play while its minion stays, the ability that made the ally a
-- minion ceases: it is its owner's ally again, its damage kept (RR Ownership and
-- Control). The minion defeated, its attachments leave first: no ally then.
revertsToAlly :: (ReverseQueue m) => Attrs Attachment -> Message -> m ()
revertsToAlly a = \case
  RemoveFromPlay t | t == toTarget a -> revert
  RemoveFromGame t | t == toTarget a -> revert
  _ -> pure ()
 where
  revert = for_ (attachedAllyMinion a) \m -> do
    inPlay <- selectAny (MinionWithId m)
    leaving <- minionLeavingPlay m
    when (inPlay && not leaving) $ minionBecomesAlly m
