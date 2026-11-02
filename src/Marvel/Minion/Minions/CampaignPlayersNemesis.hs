module Marvel.Minion.Minions.CampaignPlayersNemesis where

import Data.List (nub)
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CampaignPlayers.Cards qualified as Heroes
import Marvel.CampaignPlayers.EncounterCards qualified as Cards
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Entity
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

cn56024, cn56027, cn56028, cn56055, cn60033, cn60061, cn60063 :: MinionCard CampaignNemesisMinion
cn56024 = minion makeMinion Cards.enc56024 (Sch 1) (Atk 1) (HP 4)
cn56027 = minion makeMinion Cards.enc56027 (Sch 2) (Atk 1) (HP 3)
cn56028 = minion makeMinion Cards.enc56028 (Sch 1) (Atk 2) (HP 3)
cn56055 = minion makeMinion Cards.enc56055 (Sch 1) (Atk 1) (HP 5)
cn60033 = minion makeMinion Cards.enc60033 (Sch 1) (Atk 3) (HP 5)
cn60061 = minion makeMinion Cards.enc60061 (Sch 3) (Atk 2) (HP 6)
cn60063 = minion makeMinion Cards.enc60063 (Sch 1) (Atk 2) (HP 4)

makeMinion :: Attrs Minion -> CampaignNemesisMinion
makeMinion a = CampaignNemesisMinion (a `With` (Nothing :: Maybe IdentityId))

newtype CampaignNemesisMinion = CampaignNemesisMinion (Attrs Minion `With` Maybe IdentityId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion CampaignNemesisMinion where
  toMinionAttrs (CampaignNemesisMinion (a `With` _)) = a
  overMinionAttrs f (CampaignNemesisMinion (a `With` p)) = CampaignNemesisMinion (f a `With` p)

instance HasModifiersFor CampaignNemesisMinion where
  getModifiersFor _ (MinionRef mid) (CampaignNemesisMinion (a `With` _)) | toCardCode a == "60063" = do
    kingpin <- getsGame $ maybe False (sameCard Cards.enc60061) . (`abilityCardDef` MinionRef mid)
    pure [CannotTakeDamage | kingpin]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CampaignNemesisMinion where
  getAbilities (CampaignNemesisMinion (a `With` _)) = case toCardCode a of
    "56024" -> [windowAbility a 1 (W.EnemyAttackedAnyTarget W.After self) ForcedResponse NoCost $ runAbility a 1]
    "56028" -> [windowAbility a 1 (W.EnemyAttackedAndDamaged self AnyCharacter) ForcedResponse NoCost $ runAbility a 1]
    "56055" -> [windowAbility a 1 (W.EnemyAttacked W.When self AnyIdentity) ForcedInterrupt NoCost $ runAbility a 1]
    "60061" -> [windowAbility a 1 (W.EnemyAttacked W.When self $ IdentityIs Heroes.fne60037a) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []
   where self = EnemyWithId $ EnemyMinionId $ minionId a

instance RunMessage CampaignNemesisMinion where
  runMessage msg e@(CampaignNemesisMinion (a `With` discardedFor)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> case toCardCode a of
      "56027" -> do
        schemes <- selectList $ SideSchemeIs Cards.enc56025
        if null schemes then searchEverywhereAndRevealEncounterCard you (CardIs Cards.enc56025)
        else traverse_ (\sid -> placeThreatOn a sid 2) schemes
        pure e
      "60033" -> do
        supports <- selectList $ SupportControlledBy (IdentityWithId you) <> SupportWithTrait T.Persona
        chooseOne you $ [targeting sid (removeFromPlay sid) | sid <- supports]
          <> [labeled "$label.cnBullseyeAttacks" $ enemyAttacks mid you]
        pure e
      _ -> CampaignNemesisMinion . (`With` discardedFor) <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 windows _ -> case toCardCode a of
      "56024" -> e <$ (selectList (SideSchemeIs Cards.enc56025) >>= traverse_ (\sid -> placeThreatOn a sid 1))
      "56028" -> e <$ (selectList (SideSchemeIs Cards.enc56025) >>= traverse_ (\sid -> placeThreatOn a sid 2))
      "56055" -> case headMay [you | W.EnemyAttack _ you <- windows] of
        Nothing -> pure e
        Just you -> do
          discardFromDeck you 1 a
          pure $ CampaignNemesisMinion (a `With` Just you)
      "60061" -> do
        for_ [you | W.EnemyAttack _ you <- windows] $ replacePendingAttackWithScheme (minionId a)
        pure e
      _ -> pure e
    WithDiscarded (isTarget a -> True) FromDeck (onlyPlayerCards -> cards) | toCardCode a == "56055" -> do
      for_ discardedFor $ \you -> do
        let resources = nub [r | card <- cards, (PrintedResource, r) <- cdResources $ getCardDef card]
        when (Energy `elem` resources) $ changeAttack (minionId a) 2
        when (Mental `elem` resources) $ discardFromHand you 1
        when (Physical `elem` resources) $ enemyBecomesTough (minionId a)
        when (Wild `elem` resources) $ do
          upgrades <- selectRefs $ UpgradeControlledBy (IdentityWithId you)
          chooseAndDiscardControlled you upgrades
      pure $ CampaignNemesisMinion (a `With` Nothing)
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "56027" -> do
      schemes <- selectList $ SideSchemeIs Cards.enc56025
      if null schemes then addEnemyBoost enemy 2
      else traverse_ (\sid -> placeThreatOn a sid 2) schemes
      pure e
    _ -> CampaignNemesisMinion . (`With` discardedFor) <$> runMessage msg a
