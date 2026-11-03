module Marvel.CivilWar.ChoosingSides where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Types
import Marvel.Question (runAbility)
import Marvel.Query (selectJust)
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Window qualified as W

newtype ChoosingSides = ChoosingSides (Attrs SideScheme `With` Bool)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

choosingSides :: [SideSchemeCard ChoosingSides]
choosingSides = [sideScheme (\a -> ChoosingSides (a `With` False)) def (PerPlayer 4)
  | def <- [C.cw56128a,C.cw56206a,C.cw57039a,C.cw57081a]]

instance IsSideScheme ChoosingSides where
  toSideSchemeAttrs (ChoosingSides (a `With` _)) = a
  sideSchemeAbilities (ChoosingSides (a `With` resolving)) =
    [windowAbility a 1 (W.ThreatRemovedFromScheme $ SchemeWithId $ SchemeSideSchemeId $ sideSchemeId a)
      ForcedResponse NoCost (runAbility a 1) | sideSchemeThreat a == 0,not resolving]

instance HasModifiersFor ChoosingSides where
  getModifiersFor _ target (ChoosingSides (a `With` _))
    | isTarget a target = pure [CannotLeavePlay]
    | VillainRef vid <- target = do
        leader <- getActiveVillainIn =<< getAreaOfTarget a
        pure [AttackDamageCap 2 | leader == Just vid]
  getModifiersFor _ _ _ = pure []

instance RunMessage ChoosingSides where
  runMessage msg e@(ChoosingSides (a `With` resolving)) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ | not resolving -> do
      here <- getAreaOfTarget a
      players <- filterM (fmap (== here) . getAreaOfTarget) =<< getAllPlayers
      first <- case players of
        p : _ -> asYou p $ selectJust FirstPlayer
        [] -> getActivePlayerId
      key <- getDeckKeyFor first
      cards <- take 5 . pileDeck <$> getPile key
      cardSpecific a "dealCards" (first,key,players,cards)
      pure $ ChoosingSides (a `With` True)
    CardSpecific (isTarget a -> True) "dealCards" (fromJSON -> Success (first :: IdentityId,key :: DeckKey,players :: [IdentityId],cards :: [EncounterCard])) -> do
      case players of
        recipient : rest | not (null cards) -> lookForEnemyTeam first recipient cards
          [cardLabeled (toCardCode card) $ do
            fromDeck key $ dealFoundEncounterCard recipient card
            cardSpecific a "dealCards" (first,key,rest,filter ((/= ecCardId card) . ecCardId) cards)
          | card <- cards]
        _ -> do
          fromDeck key shuffleEncounterDeck
          flipSideSchemeToObligation (sideSchemeId a) first (flipCardCode $ toCardCode a)
      pure e
    _ -> (\updated -> ChoosingSides (updated `With` resolving)) <$> runMessage msg a

newtype NowItsPersonal = NowItsPersonal (Attrs Obligation)
  deriving anyclass (IsObligation,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

nowItsPersonal :: [ObligationCard NowItsPersonal]
nowItsPersonal = map (obligation NowItsPersonal) [C.cw56128b,C.cw56206b,C.cw57039b,C.cw57081b]

instance HasAbilities NowItsPersonal where
  getAbilities (NowItsPersonal a) =
    [ability a 1 Action NoCriteria (RemoveFromGameCost $ toRef a) (runAbility a 1) | isJust (obligationOwner a)]

instance RunMessage NowItsPersonal where
  runMessage msg e@(NowItsPersonal a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      cardSpecific GameRef "cwBeginLeaderRewards" you
      pure e
    ObligationMessage oid ResolvedObligation{} | oid == obligationId a -> pure e
    _ -> NowItsPersonal <$> runMessage msg a
