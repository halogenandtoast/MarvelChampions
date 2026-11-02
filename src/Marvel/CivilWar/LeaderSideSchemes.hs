module Marvel.CivilWar.LeaderSideSchemes where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.CivilWar.LeaderTreacheries (addCaptainMarvelEnergy)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.EncounterSet qualified as E
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Window qualified as W

newtype LeaderSideScheme = LeaderSideScheme (Attrs SideScheme `With` Maybe (VillainId,AttachmentId))
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

leaderSideSchemeDefinitions :: [CardDef]
leaderSideSchemeDefinitions = [C.cw56072,C.cw56104,C.cw56149,C.cw56179,C.cw57012,C.cw57054]

leaderSideSchemes :: [SideSchemeCard LeaderSideScheme]
leaderSideSchemes = map (\def -> sideScheme (\a -> LeaderSideScheme (a `With` Nothing)) def $ PerPlayer 3) leaderSideSchemeDefinitions

instance IsSideScheme LeaderSideScheme where
  toSideSchemeAttrs (LeaderSideScheme (a `With` _)) = a
  sideSchemeAbilities (LeaderSideScheme (a `With` host)) = case toCardCode a of
    "56179" -> [limitedWindowAbility a 1 (W.VillainWouldTakeDamageFrom (VillainWithId vid) DamageFromAnAttack) ForcedInterrupt
      (AttachmentExists $ AttachmentMatches [AttachmentWithId aid,AttachmentWithCounters $ GreaterThan $ Static 0]) NoCost (runAbility a 1) | Just (vid,aid) <- [host]]
    "57012" -> [windowAbility a 2 (W.IdentityChangedAnyForm W.After You) AlterEgoResponse (ExhaustCharacterCost $ IdentityCharacters You) (runAbility a 2)]
    _ -> []

instance HasModifiersFor LeaderSideScheme where
  getModifiersFor _ target (LeaderSideScheme (a `With` _))
    | isTarget a target,toCardCode a == "57012" = pure [ThreatCannotBeRemovedByHeroOrAlly]
  getModifiersFor _ _ _ = pure []

instance RunMessage LeaderSideScheme where
  runMessage msg e@(LeaderSideScheme (a `With` host)) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a,toCardCode a == "56179" -> do
      leader <- leaderOfFamily E.CwSpiderWoman
      finesse <- selectOne $ AttachmentIs C.cw56174
      pure $ LeaderSideScheme (a `With` ((,) <$> leader <*> finesse))
    AttachmentMessage aid (AttachedToEnemy (EnemyVillainId vid)) | toCardCode a == "56179" -> do
      finesse <- getsGame $ maybe False ((== "56174") . toCardCode) . (`abilityCardDef` AttachmentRef aid)
      spiderWoman <- getsGame $ maybe False ((== Just E.CwSpiderWoman) . cdEncounterSet) . (`abilityCardDef` VillainRef vid)
      same <- (==) <$> getAreaOfTarget a <*> getAreaOfTarget vid
      pure $ if finesse && spiderWoman && same then LeaderSideScheme (a `With` Just (vid,aid)) else e
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      blanked <- isBlanked a
      unless blanked $ do
        you <- getDefeatedBy a
        case toCardCode a of
          "56072" -> do
            deck <- asYou you getEncounterDeck
            discard <- asYou you getEncounterDiscard
            key <- getDeckKeyFor you
            let choices = filter (\card -> cdEncounterSet (getCardDef card) == Just E.CwIronMan && cdCardType (getCardDef card) == AttachmentType) (deck <> discard)
            lookForEnemyTeam you a choices [cardLabeled (toCardCode card) $ revealEncounterCard you card | card <- choices]
            fromDeck key shuffleEncounterDeck
          "56104" -> addCaptainMarvelEnergy 3
          "56149" -> leaderOfFamily E.CwCaptainAmerica >>= traverse_ (\vid -> do
            returnCaptainAmericaShield you vid
            enemyActivationAfterCurrent vid $ villainActivatesAgainst vid you)
          "57054" -> do
            old <- getCharacterStatusCount (IdentityCharacter you) ConfusedStatus
            confuseIdentity you
            cardSpecific a "passingThroughAfterConfuse" (you,old)
          _ -> pure ()
      (\updated -> LeaderSideScheme (updated `With` host)) <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      n <- maybe (pure 0) (getCountersOn . snd) host
      for_ [vid | W.VillainDamageFrom vid _ _ <- windows] $ \vid -> preventEnemyDamage (EnemyVillainId vid) n
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> e <$ removeSchemeThreat a (SchemeSideSchemeId $ sideSchemeId a) 3
    CardSpecific (isTarget a -> True) "passingThroughAfterConfuse" (fromJSON -> Success (you,old :: Natural)) -> do
      current <- getCharacterStatusCount (IdentityCharacter you) ConfusedStatus
      when (current <= old) $ stunIdentity you
      pure e
    _ -> (\updated -> LeaderSideScheme (updated `With` host)) <$> runMessage msg a
