module Marvel.FearNoEvil.TracksuitMafia where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

data Tracksuit = Tracksuit (Attrs Minion) Natural
  deriving stock (Show, Eq)
instance IsMinion Tracksuit where
  toMinionAttrs (Tracksuit a _) = a
  overMinionAttrs f (Tracksuit a n) = Tracksuit (f a) n
  minionDisplayCounters (Tracksuit _ n) = n
  minionAccelerationTokens (Tracksuit _ n) = n
instance HasCardCode Tracksuit where
  toCardCode (Tracksuit a _) = toCardCode a
instance IsRef Tracksuit where
  toRef (Tracksuit a _) = toRef a
instance ToJSON Tracksuit where
  toJSON (Tracksuit a n) = case toJSON a of
    Object o -> Object $ KeyMap.insert "tracksuitAccelerationTokens" (toJSON n) o
    value -> value
instance FromJSON Tracksuit where
  parseJSON = withObject "Tracksuit" $ \o -> Tracksuit <$> parseJSON (Object o) <*> o .:? "tracksuitAccelerationTokens" .!= 0
instance HasModifiersFor Tracksuit
tracksuits :: [MinionCard Tracksuit]
tracksuits = [minion (`Tracksuit` 0) def (Sch sch) (Atk atk) (HP hp) | (def,sch,atk,hp) <-
  [(Cards.street60200,0,2,5),(Cards.street60201,0,1,6),(Cards.street60202,3,0,7),(Cards.street60203,1,2,3)]]
instance HasAbilities Tracksuit where
  getAbilities (Tracksuit a _) =
    [windowAbility a 1 (W.MinionHasSchemed W.After $ MinionWithId $ minionId a) ForcedResponse NoCost (runAbility a 1) | toCardCode a `elem` ["60200","60201"]]
    <> [windowAbility a 2 (W.MinionDefeated W.When $ MinionWithId $ minionId a) WhenRevealed NoCost (runAbility a 2) | toCardCode a `elem` ["60202","60203"]]
instance RunMessage Tracksuit where
  runMessage msg e@(Tracksuit a n) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion _) | mid == minionId a -> do
      attrs <- runMessage msg a
      pure $ Tracksuit attrs (n + if toCardCode a == "60200" then 1 else 0)
    RanAbility _ (isTarget a -> True) 1 _ _ ->
      if toCardCode a == "60200" then pure $ Tracksuit a (n+1)
      else dealBoost (minionId a) >> pure e
    RanAbility _ (isTarget a -> True) 2 windows _ -> do
      scheme <- selectAny (SideSchemeIs Cards.street60204)
      if scheme then minionLeavingPlayStep (minionId a) "tracksuitTuckAfterLeave"
      else if toCardCode a == "60203" then minionLeavingPlayStep (minionId a) "tracksuitShuffleAfterLeave"
      else for_ [source | W.DefeatedMinion _ _ source _ <- windows] $ \source -> do
        character <- case source of
          IdentityRef you -> pure $ Just $ IdentityCharacter you
          AllyRef aid -> pure $ Just $ AllyCharacter aid
          MinionRef mid -> pure $ Just $ MinionCharacter mid
          VillainRef vid -> pure $ Just $ VillainCharacter vid
          CharacterRef c -> pure $ Just c
          _ -> fmap IdentityCharacter <$> getSourceController source
        for_ character $ \c -> giveStatus c StunnedStatus >> giveStatus c ConfusedStatus
      pure e
    CardSpecific (isTarget a -> True) "tracksuitTuckAfterLeave" _ -> do
      schemes <- selectList (SideSchemeIs Cards.street60204)
      chooseOrRunOne (minionEngagedIdentity a) [targeting sid do
        removeMinionFromGameWithoutLeaveWindow (minionId a)
        placeEncounterCardUnderSideScheme sid physicalCard | sid <- schemes]
      pure e
    CardSpecific (isTarget a -> True) "tracksuitShuffleAfterLeave" _ -> do
      removeMinionFromGameWithoutLeaveWindow (minionId a)
      shuffleIntoEncounterDeck [physicalCard]
      pure e
    _ -> (`Tracksuit` n) <$> runMessage msg a
   where
    physicalCard = MkEncounterCard (CardId $ unMinionId $ minionId a) (getCardDef a)

newtype TracksuitMafia = TracksuitMafia (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass HasModifiersFor
instance IsSideScheme TracksuitMafia where
  sideSchemeAbilities = getAbilities
tracksuitMafia :: SideSchemeCard TracksuitMafia
tracksuitMafia = sideScheme TracksuitMafia Cards.street60204 (PerPlayer 3)
instance HasAbilities TracksuitMafia where
  getAbilities (TracksuitMafia a) =
    [windowAbility a 1 (W.EncounterCardRevealedFromDeck W.After $ EncounterCardMatching $ CardWithType MinionType <> CardWithTrait T.Tracksuit) ForcedResponse NoCost (runAbility a 1)]
instance RunMessage TracksuitMafia where
  runMessage msg e@(TracksuitMafia a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait T.Tracksuit) a
      pure e
    WithDiscardedMatch (isTarget a -> True) FromEncounterDeck (EncounterCard card) -> placeEncounterCardUnderSideScheme (sideSchemeId a) card >> pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      let who = fromMaybe you $ headMay [p | W.EncounterCardRevealed W.RevealedFromEncounterDeck p _ <- windows]
      chooseOne who [cardLabeled (toCardCode c) (revealEncounterCardUnderSideScheme (sideSchemeId a) who c) | c <- sideSchemeHeldEncounterCards a]
      pure e
    _ -> TracksuitMafia <$> runMessage msg a
