module Marvel.CivilWar.Leaders where

import Marvel.Prelude
import Data.List (find)
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.CivilWar.Types qualified as CW
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.EncounterSet qualified as E
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype CivilWarLeader = CivilWarLeader (Attrs Villain)
  deriving anyclass IsVillain
  deriving newtype (Show,Eq,ToJSON,FromJSON)

leaderCards :: [VillainCard CivilWarLeader]
leaderCards = map leaderCard $ filter ((== VillainType) . cdCardType) C.civilWarDefinitions

leaderCard :: CardDef -> VillainCard CivilWarLeader
leaderCard def = let (sch,atk,hp,stage) = C.printedLeaderStats (toCardCode def)
  in villainWith CivilWarLeader def (Sch sch) (Atk atk) (HP $ PerPlayer hp) (stageL .~ stage)

leaderFamily :: Attrs Villain -> E.EncounterSet
leaderFamily = fromMaybe (error "Civil War leader has no encounter set") . cdEncounterSet . getCardDef

instance HasModifiersFor CivilWarLeader where
  getModifiersFor _ target (CivilWarLeader a)
    | isTarget a target, leaderFamily a == E.CwIronMan = do
        tech <- selectList $ AttachmentOnEnemy (toEnemyId a) <> AttachmentWithTrait T.Tech
        suits <- filterM (fmap (== Just (toEnemyId a)) . getUpgradeAttachedEnemy) =<< selectList (UpgradeNamed "High-Tech Suit")
        pure [SchemeModifier $ min 3 $ fromIntegral (length tech + length suits)]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CivilWarLeader where
  getAbilities (CivilWarLeader a) =
    [windowAbility a 1 (W.VillainRevealed W.When (VillainWithId $ villainId a) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 1)
    | villainStage a /= 1]
    <> case leaderFamily a of
      E.CwCaptainMarvel -> [windowAbility a 2 (W.CharacterDefeatedByEnemyAttack W.After $ EnemyWithId $ toEnemyId a) ForcedResponse NoCost (runAbility a 2)]
      E.CwSpiderWoman -> [windowAbility a 2 (W.EnemyAttackedAnyTarget W.When $ EnemyWithId $ toEnemyId a) ForcedInterrupt NoCost (runAbility a 2)]
      E.CwVision -> [limitedWindowAbility a 4 W.AfterVillainThreatStep ForcedResponse
        (AnyCriteria [AttachmentExists $ AttachmentOnEnemy (toEnemyId a) <> AttachmentIs form | form <- [C.cw57046a,C.cw57046b]]) NoCost (runAbility a 4)]
      E.CwSheHulk -> [windowAbility a 2 (W.IdentityChangedToForm W.After HeroIdentity) ForcedResponse NoCost (runAbility a 2)]
      _ -> []

instance RunMessage CivilWarLeader where
  runMessage msg e@(CivilWarLeader a) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == villainId a ->
      if odd (villainStage a) then do
        let next = fromMaybe (error "missing Civil War leader stage") $ find (\def -> cdEncounterSet def == cdEncounterSet (getCardDef a) && cdCardType def == VillainType && let (_,_,_,stage) = C.printedLeaderStats (toCardCode def) in stage == villainStage a + 1) C.civilWarDefinitions
        advanceVillain vid
        pure $ advanceVillainTo (leaderCard next) a
      else do
        board <- teamForBoard vid
        case board of
          Just team -> cardSpecific GameRef "cwFinishBoard" (team,CW.EnemyLeaderDefeated) >> pure e
          Nothing -> CivilWarLeader <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      if even (villainStage a) then do
        traverse_ dealEncounterCard =<< playersAtLeader (villainId a)
        lastingModifiers a (SourceEntity $ toSource a) [CannotTakeDamage] False
      else cardSpecific a "cwLeaderSetup" ()
      pure e
    CardSpecific (isTarget a -> True) "cwLeaderSetup" _ -> do
      you <- firstPlayerAtLeader (villainId a)
      case leaderFamily a of
        E.CwIronMan -> do
          deck <- asYou you getEncounterDeck
          key <- getDeckKeyFor you
          let choices = filter (\c -> cdEncounterSet (getCardDef c) == Just E.CwIronMan && cdCardType (getCardDef c) == AttachmentType) deck
          lookForEnemyTeam you a choices [cardLabeled (toCardCode card) $ revealEncounterCard you card | card <- choices]
          fromDeck key shuffleEncounterDeck
        family -> for_ (setupAttachment family) $ \code -> do
          key <- getDeckKeyFor you
          fromDeck key $ searchEverywhereAndRevealEncounterCard you (CardIs $ civilWarDef code)
      pure e
    RanAbility _ (isTarget a -> True) 2 windows _ -> do
      case leaderFamily a of
        E.CwCaptainMarvel -> selectList (AttachmentIs C.cw56098) >>= traverse_ (\aid -> cardSpecific aid "cwEnergy" (1 :: Natural))
        E.CwSpiderWoman -> do
          finesse <- selectList $ AttachmentIs C.cw56174
          n <- sum <$> traverse getCountersOn finesse
          changeAttack (villainId a) (fromIntegral n)
          for_ finesse $ \aid -> removeCounters aid =<< getCountersOn aid
          cardSpecific (villainId a) "cwFinesseSpent" n
        E.CwSheHulk -> for_ [you | W.IdentityChangesForm you <- windows] $ \you -> damageIdentity you a (toDamage 1 FromAbility)
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "cwShieldAttached" _ -> do
      blanked <- isBlanked a
      when (leaderFamily a == E.CwCaptainAmerica && not blanked) $ do
        players <- playersAtLeader (villainId a)
        if null players then giveStatus (villainId a) ToughStatus else do
          you <- firstPlayerAtLeader (villainId a)
          chooseOnePromptedAbout you "$label.resolveForcedResponse" a [targeting a $ giveStatus (villainId a) ToughStatus]
      pure e
    RanAbility _ (isTarget a -> True) 4 _ _ -> do
      flipLeaderMassForm (toEnemyId a)
      pure e
    _ -> CivilWarLeader <$> runMessage msg a

setupAttachment :: E.EncounterSet -> Maybe CardCode
setupAttachment = \case
  E.CwCaptainMarvel -> Just "56098"
  E.CwCaptainAmerica -> Just "56143"
  E.CwSpiderWoman -> Just "56174"
  E.CwSheHulk -> Just "57007"
  E.CwVision -> Just "57046a"
  _ -> Nothing
