module Marvel.AlterEgo.AlterEgos.WonderHerculesPack where

import Marvel.Prelude
import System.Random.Shuffle (shuffleM)
import Data.Aeson.KeyMap qualified as KM
import Marvel.Ability
import Marvel.AlterEgo.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hercules.Cards qualified as HC
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.WonderMan.Cards qualified as WM

wm58001b :: AlterEgoCard WmHcAlterEgo
wm58001b = alterEgo (\attrs -> WmHcAlterEgo attrs [] []) WM.wm58001b (HP $ Static 12) (HandSize 6) (Rec 3) [WM.wm58025]

hc59001b :: AlterEgoCard WmHcAlterEgo
hc59001b = alterEgo (\attrs -> WmHcAlterEgo attrs [] []) HC.hc59001b (HP $ Static 14) (HandSize 6) (Rec 4) [HC.hc59035]

data WmHcAlterEgo = WmHcAlterEgo (Attrs AlterEgo) [EncounterCard] [PlayerCard]
  deriving stock (Show, Eq)
  deriving anyclass HasModifiersFor

instance IsAlterEgo WmHcAlterEgo where
  toAlterEgoAttrs (WmHcAlterEgo attrs _ _) = attrs
instance IsRef WmHcAlterEgo where
  toRef (WmHcAlterEgo attrs _ _) = toRef attrs
instance ToJSON WmHcAlterEgo where
  toJSON (WmHcAlterEgo attrs labor gifts) = case toJSON attrs of
    Object o -> Object $ KM.insert "laborDeck" (toJSON labor) $ KM.insert "giftDeck" (toJSON gifts) o
    other -> other
instance FromJSON WmHcAlterEgo where
  parseJSON value = withObject "WmHcAlterEgo" (\o -> WmHcAlterEgo <$> parseJSON value <*> o .:? "laborDeck" .!= [] <*> o .:? "giftDeck" .!= []) value

instance HasAbilities WmHcAlterEgo where
  getAbilities a@(WmHcAlterEgo attrs _ _) = case toCardCode attrs of
    "58001b" -> [ability a 1 Setup NoCriteria NoCost $ runAbility a 1]
    _ -> [ ability a 1 Setup NoCriteria NoCost $ runAbility a 1
         , ability a 2 Action (IsSelf <> CanRevealHerculesLabor) NoCost $ runAbility a 2]

instance RunMessage WmHcAlterEgo where
  runMessage msg ae@(WmHcAlterEgo attrs labor gifts) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      case toCardCode attrs of
        "58001b" -> newPlayerCard you WM.wm58002 >>= putIntoPlay you
        _ -> do
          labor' <- shuffleM =<< traverse genEncounterCard [HC.hc59002, HC.hc59003, HC.hc59004]
          gifts' <- shuffleM =<< traverse (newPlayerCard you) [HC.hc59005, HC.hc59006, HC.hc59007]
          pure ()
          cardSpecific you "hcSetDecks" (labor', gifts')
      pure ae
    RanAbility you (isTarget attrs -> True) 2 _ _ -> ae <$ cardSpecific you "hcRevealLabor" ()
    CardSpecific (IdentityRef you) "hcSetDecks" (fromJSON -> Success (labor', gifts')) | you == alterEgoIdentityId attrs && toCardCode attrs == "59001b" -> pure $ WmHcAlterEgo attrs labor' gifts'
    CardSpecific (IdentityRef you) "hcRevealLabor" _ | you == alterEgoIdentityId attrs && toCardCode attrs == "59001b" -> do
      attachments <- selectList $ AttachmentWithTrait T.Labor
      obligations <- getObligationsOf you
      active <- or <$> traverse (\target -> getsGame $ maybe False (member T.Labor . cdTraits) . (`abilityCardDef` target)) (map AttachmentRef attachments <> map ObligationRef obligations)
      case labor of
        card : rest | not active -> do
          revealEncounterCard you card
          pure $ WmHcAlterEgo attrs rest gifts
        _ -> pure ae
    CardSpecific (IdentityRef you) "hcGainGift" _ | you == alterEgoIdentityId attrs && toCardCode attrs == "59001b" -> case gifts of
      card : rest -> do
        putIntoPlay you card
        pure $ WmHcAlterEgo attrs labor rest
      _ -> pure ae
    CardSpecific (IdentityRef you) "hcReturnLabor" (fromJSON -> Success card) | you == alterEgoIdentityId attrs && toCardCode attrs == "59001b" -> pure $ WmHcAlterEgo attrs (if card `elem` labor then labor else labor <> [card]) gifts
    _ -> (\attrs' -> WmHcAlterEgo attrs' labor gifts) <$> runMessage msg attrs

newPlayerCard :: MonadRandom m => IdentityId -> CardDef -> m PlayerCard
newPlayerCard you def = (\cid -> MkPlayerCard cid def (Just you) (Just you)) <$> getRandom
