-- | The Infinity Gauntlet (The Mad Titan's Shadow), its own modular set: usable in any
-- scenario with exactly one villain at the start (MTS rulebook p.16, and errata).
module Marvel.Attachment.Attachments.InfinityGauntlet (
  infinityGauntlet,
  InfinityGauntlet (..),
) where

import Marvel.Prelude

import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Ability
import Marvel.Environment.Environments.InfinityStones (infinityStonesInPlay, resolveInfinityStone)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Window qualified as W

infinityGauntlet :: AttachmentCard InfinityGauntlet
infinityGauntlet = attachment InfinityGauntlet Cards.infinityGauntlet

newtype InfinityGauntlet = InfinityGauntlet (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK, +1 SCH to the villain it is attached to.
instance HasModifiersFor InfinityGauntlet where
  getModifiersFor _ (VillainRef vid) (InfinityGauntlet a)
    | attachmentEnemy a == Just (EnemyVillainId vid) = pure [AttackModifier 1, SchemeModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Permanent. Setup: attach to the villain; the six Infinity Stones, taken from the
-- encounter deck, are the Infinity Stone deck. With more than one villain at the start
-- the set cannot be used: the Gauntlet and its stones are put out of play. With none yet
-- (Loki's scenario, whose setup reveals him), it waits for the villain to enter play.
-- Leaving what it was on, it attaches to the villain again (RR Permanent).
--
-- Forced Response: after attached villain activates against you, resolve the Special
-- ability of each Infinity Stone in play, in the order they entered play. Otherwise, put
-- the top card of the Infinity Stone deck into play.
instance RunMessage InfinityGauntlet where
  runMessage msg a@(InfinityGauntlet attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      made <- elem InfinityStoneDeckKey <$> getDeckKeys
      if made
        then attachToTheVillain attrs
        else do
          villains <- selectList AnyVillain
          stones <- filter (cardMatch $ CardWithType EnvironmentType <> CardWithTrait InfinityStone) <$> getEncounterDeck
          case villains of
            [v] -> do
              attachTo (attachmentId attrs) v
              createDeck InfinityStoneDeckKey stones
            [] -> createDeck InfinityStoneDeckKey stones
            _ -> do
              removeFromGame attrs
              setAside (map EncounterCard stones)
      pure a
    -- the villain revealed by setup, the Gauntlet waiting for him
    VillainMessage vid SetVillainHp | not (attached attrs) -> do
      attachTo (attachmentId attrs) vid
      pure a
    CheckWindows ws
      | Just enemy <- attachmentEnemy attrs
      , you : _ <- [p | W.Window W.After (W.EnemyActivation e p) <- ws, e == enemy] -> do
          -- the player the activation eliminated has no Specials to resolve
          inGame <- elem you <$> getPlayers
          when inGame do
            stones <- infinityStonesInPlay
            if null stones
              then putTopInfinityStoneIntoPlay you
              else for_ stones (resolveInfinityStone you)
          pure a
    _ -> InfinityGauntlet <$> runMessage msg attrs
