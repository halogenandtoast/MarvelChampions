module Marvel.Villain.Villains.Nebula (
  nebula,
  nebula1,
  nebula2,
  nebula3,
  Nebula (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Attachment.Attachments.Techniques (resolveSpecial)
import Marvel.Card
import Marvel.Deck
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window qualified as W

-- | Whether a Technique attachment has gained surge this round.
newtype Meta = Meta {techniqueSurged :: Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype Nebula = Nebula (Attrs Villain `With` Meta)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsVillain Nebula where
  toVillainAttrs (Nebula (attrs `With` _)) = attrs
  overVillainAttrs f (Nebula (attrs `With` meta)) = Nebula (f attrs `With` meta)

nebula :: VillainCard Nebula
nebula = nebula1

fresh :: Attrs Villain -> Nebula
fresh = Nebula . (`With` Meta False)

nebula1 :: VillainCard Nebula
nebula1 = villain fresh Cards.nebula1 (Sch 1) (Atk 2) (HP $ PerPlayer 14)

nebula2 :: VillainCard Nebula
nebula2 = villainWith fresh Cards.nebula2 (Sch 2) (Atk 2) (HP $ PerPlayer 17) (stageL .~ 2)

nebula3 :: VillainCard Nebula
nebula3 = villainWith fresh Cards.nebula3 (Sch 2) (Atk 3) (HP $ PerPlayer 20) (stageL .~ 3)

isTechnique :: EncounterCard -> Bool
isTechnique = cardMatch (CardWithType AttachmentType <> CardWithTrait Technique)

-- | Each stage: "The first Technique attachment revealed each round gains surge", and
-- "[star] Forced Interrupt: When Nebula initiates an activation against you, resolve the
-- Special ability on each Technique attachment in play", then (I) discard each of them,
-- (II) choose and discard 1 of them, (III) you may remove the top card of your deck from
-- the game to choose and discard 1 of them. A stunned attack or confused scheme is not
-- initiated, its status card removed instead (RR Stun, Confuse).
instance RunMessage Nebula where
  runMessage msg e@(Nebula (attrs `With` meta)) = runQueueT $ case msg of
    VillainMessage ident msg' | ident == villainId attrs -> case msg' of
      VillainDefeated -> do
        difficulty <- getDifficulty
        case (difficulty, villainStage attrs) of
          (_, 1) -> advance nebula2
          (Expert, 2) -> advance nebula3
          _ -> passOn
      VillainAttacks you | not (villainStunned attrs) -> interrupt you >> passOn
      VillainAttacksWithoutBoost you | not (villainStunned attrs) -> interrupt you >> passOn
      VillainSchemes | not (villainConfused attrs) -> (interrupt =<< getActivePlayerId) >> passOn
      _ -> passOn
    CardSpecific (isTarget attrs -> True) "nebulaDiscardTechniques" (fromJSON -> Success (you, techniques)) -> do
      inPlay <- filterM (selectAny . AttachmentWithId) techniques
      let discard aid = removeFromPlay (AttachmentRef aid)
          choices = [targeting (AttachmentRef aid) (discard aid) | aid <- inPlay]
      unless (null inPlay) case villainStage attrs of
        1 -> for_ inPlay discard
        2 -> chooseOnePrompted you "$label.chooseTechniqueToDiscard" choices
        _ ->
          projectMap PlayerIdentityDeck (headMay . unDeck) you >>= traverse_ \top ->
            chooseOnePrompted you "$label.removeTopCardToDiscardTechnique" $
              labeled "$label.doNotRemoveTopCard" (pure ())
                : [targeting (AttachmentRef aid) (removeFromGame (CardIdRef $ pcCardId top) >> discard aid) | aid <- inPlay]
      pure e
    CheckWindows [W.Window W.When (W.EncounterCardRevealed _ you card)] | isTechnique card, not (techniqueSurged meta) -> do
      -- after the card has resolved
      afterMatching (== RevealedEncounterCard you card) (surge you)
      Nebula . (`With` Meta True) <$> runMessage msg attrs
    EndRound -> Nebula . (`With` Meta False) <$> runMessage msg attrs
    _ -> passOn
   where
    passOn = Nebula . (`With` meta) <$> runMessage msg attrs
    advance stage = do
      advanceVillain (villainId attrs)
      pure $ overVillainAttrs (const $ toVillainAttrs $ advanceVillainTo stage attrs) e
    interrupt you = do
      techniques <- selectList (AttachmentWithTrait Technique)
      unless (null techniques) do
        for_ techniques (`resolveSpecial` you)
        cardSpecific attrs "nebulaDiscardTechniques" (you, techniques)
