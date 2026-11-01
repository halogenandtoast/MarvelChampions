module Marvel.Villain.Villains.AgeOfApocalypseEnSabahNur where

import Marvel.Prelude
import Marvel.Id ()
import Marvel.Card.Code
import Marvel.Matchers
import Marvel.Query ()
import Data.Text (isSuffixOf)
import Control.Monad (replicateM_)
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Modifier
import Marvel.Status
import Marvel.Card
import Marvel.Entity
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype AoaEnSabahNurVillain = AoaEnSabahNurVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

enSabahNurBiomorph1 :: VillainCard AoaEnSabahNurVillain
enSabahNurBiomorph1 = villainWith AoaEnSabahNurVillain Cards.enSabahNurBiomorph1 (Sch 1) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 1)

enSabahNurCyberpath1 :: VillainCard AoaEnSabahNurVillain
enSabahNurCyberpath1 = villainWith AoaEnSabahNurVillain Cards.enSabahNurCyberpath1 (Sch 2) (Atk 1) (HP $ PerPlayer 16) (stageL .~ 1)

enSabahNurGiant1 :: VillainCard AoaEnSabahNurVillain
enSabahNurGiant1 = villainWith AoaEnSabahNurVillain Cards.enSabahNurGiant1 (Sch 2) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 1)

enSabahNurBiomorph2 :: VillainCard AoaEnSabahNurVillain
enSabahNurBiomorph2 = villainWith AoaEnSabahNurVillain Cards.enSabahNurBiomorph2 (Sch 1) (Atk 3) (HP $ PerPlayer 20) (stageL .~ 2)

enSabahNurCyberpath2 :: VillainCard AoaEnSabahNurVillain
enSabahNurCyberpath2 = villainWith AoaEnSabahNurVillain Cards.enSabahNurCyberpath2 (Sch 3) (Atk 1) (HP $ PerPlayer 20) (stageL .~ 2)

enSabahNurGiant2 :: VillainCard AoaEnSabahNurVillain
enSabahNurGiant2 = villainWith AoaEnSabahNurVillain Cards.enSabahNurGiant2 (Sch 2) (Atk 3) (HP $ PerPlayer 20) (stageL .~ 2)

enSabahNurBiomorph3 :: VillainCard AoaEnSabahNurVillain
enSabahNurBiomorph3 = villainWith AoaEnSabahNurVillain Cards.enSabahNurBiomorph3 (Sch 2) (Atk 3) (HP $ PerPlayer 24) (stageL .~ 3)

enSabahNurCyberpath3 :: VillainCard AoaEnSabahNurVillain
enSabahNurCyberpath3 = villainWith AoaEnSabahNurVillain Cards.enSabahNurCyberpath3 (Sch 3) (Atk 2) (HP $ PerPlayer 24) (stageL .~ 3)

enSabahNurGiant3 :: VillainCard AoaEnSabahNurVillain
enSabahNurGiant3 = villainWith AoaEnSabahNurVillain Cards.enSabahNurGiant3 (Sch 3) (Atk 3) (HP $ PerPlayer 24) (stageL .~ 3)

instance HasModifiersFor AoaEnSabahNurVillain where
  getModifiersFor _ target (AoaEnSabahNurVillain attrs) = pure [KeywordModifier Overkill | isTarget attrs target, formOf attrs == "Biomorph"]

formOf :: Attrs Villain -> Text
formOf attrs = case unCardCode (toCardCode $ getCardDef attrs) of
  value | "a" `isSuffixOf` value -> "Biomorph"
  value | "b" `isSuffixOf` value -> "Cyberpath"
  _ -> "Giant"
instance RunMessage AoaEnSabahNurVillain where
  runMessage msg a@(AoaEnSabahNurVillain attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "aoaChangeApocalypseForm" (fromJSON -> Success form)
      | form /= formOf attrs -> do
          cardSpecific attrs "aoaResolveApocalypseForm" form
          when (form == ("Giant" :: Text)) $ do
            replicateM_ (fromEnum (villainStunned attrs) + fromEnum (villainAdditionalStun attrs)) $ removeStatus (villainId attrs) StunnedStatus
            replicateM_ (fromEnum (villainConfused attrs) + fromEnum (villainAdditionalConfuse attrs)) $ removeStatus (villainId attrs) ConfusedStatus
          pure $ flipVillainTo (formCard (villainStage attrs) form) attrs
    CardSpecific (isTarget attrs -> True) "aoaResolveApocalypseForm" (fromJSON -> Success form) -> do
      let amount = villainStage attrs
      case (form :: Text) of
        "Biomorph" -> getPlayers >>= traverse_ (\you -> dealIndirectDamage you attrs amount)
        "Cyberpath" -> placeThreatOnEach attrs amount AnyScheme
        "Giant" -> heal (villainId attrs) amount
        _ -> pure ()
      pure a
    VillainMessage vid VillainDefeated | vid == villainId attrs -> do
      difficulty <- getDifficulty
      case villainStage attrs of
        1 -> nextStage enSabahNurBiomorph2
        2 | difficulty == Expert -> nextStage enSabahNurBiomorph3
        _ -> AoaEnSabahNurVillain <$> runMessage msg attrs
    _ -> AoaEnSabahNurVillain <$> runMessage msg attrs
   where
    nextStage card = do
      advanceVillain (villainId attrs)
      pure $ overVillainAttrs (\new -> new {villainCounters = villainCounters attrs}) $ advanceVillainTo card attrs

formCard :: Natural -> Text -> VillainCard AoaEnSabahNurVillain
formCard stage form = case (stage, form) of
  (1, "Biomorph") -> enSabahNurBiomorph1
  (1, "Cyberpath") -> enSabahNurCyberpath1
  (1, _) -> enSabahNurGiant1
  (2, "Biomorph") -> enSabahNurBiomorph2
  (2, "Cyberpath") -> enSabahNurCyberpath2
  (2, _) -> enSabahNurGiant2
  (_, "Biomorph") -> enSabahNurBiomorph3
  (_, "Cyberpath") -> enSabahNurCyberpath3
  _ -> enSabahNurGiant3
