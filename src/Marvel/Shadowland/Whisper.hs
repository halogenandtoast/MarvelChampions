module Marvel.Shadowland.Whisper where

import Data.HashSet qualified as HashSet
import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Hp
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Shadowland (proteges)
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Shadowland.ProtegeAbilities
import Marvel.Stats
import Marvel.Trait qualified as T

-- A second entity registry for the same six physical villain cards, while Whisper
-- treats them as minions. Their normal printed abilities continue to function.
newtype WhisperMinion = WhisperMinion (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsMinion, HasModifiersFor)
whisperMinionDef :: CardDef -> CardDef
whisperMinionDef def = def {cdCardType=MinionType,cdTraits=HashSet.insert T.Elite (cdTraits def),cdKeywords=fromList [Guard,Villainous]}
whisperMinions :: [MinionCard WhisperMinion]
whisperMinions = [minion WhisperMinion (whisperMinionDef def) (Sch sch) (Atk atk) (HP 6) | (def,sch,atk) <-
  [(Cards.street65012a,2,1),(Cards.street65013a,1,2),(Cards.street65014a,1,2),(Cards.street65015a,1,2),(Cards.street65016a,2,1),(Cards.street65017a,1,0)]]
instance HasAbilities WhisperMinion where
  getAbilities (WhisperMinion a) = protegeAbilities a (toCardCode a) (toEnemyId a)
instance RunMessage WhisperMinion where
  runMessage msg e@(WhisperMinion a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) index windows _ -> resolveProtegeAbility a (toCardCode a) (toEnemyId a) (minionAttacking a) you index windows >> pure e
    WithDiscarded (isTarget a -> True) _ cards -> resolveElektraDiscard (toEnemyId a) cards >> pure e
    MinionMessage mid MinionDefeated | mid == minionId a -> do
      let original = headMay [def | def <- proteges,toCardCode def == toCardCode a]
      for_ original $ \def -> case toCard a of
        EncounterCard card -> setAside [EncounterCard card {ecCardDef=def}]
        _ -> pure ()
      pure e
    _ -> WhisperMinion <$> runMessage msg a

newtype Whisper = Whisper (Attrs Attachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor)
whisper :: AttachmentCard Whisper
whisper = attachment Whisper Cards.street65040
instance RunMessage Whisper where
  runMessage msg e@(Whisper a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      corridors <- selectAny (VillainWithCardCode "65037")
      if corridors then cardSpecific GameRef "slWhisperChosen" (you,aid)
        else setAside [toCard a] >> (selectOne ActiveVillain >>= traverse_ (\vid -> villainSchemesAgainst vid you))
      pure e
    _ -> Whisper <$> runMessage msg a
