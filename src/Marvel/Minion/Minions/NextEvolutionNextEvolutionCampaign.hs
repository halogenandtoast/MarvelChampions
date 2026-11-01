module Marvel.Minion.Minions.NextEvolutionNextEvolutionCampaign where

import Marvel.Minion.Runner
import Marvel.Prelude
import Marvel.Card
import Marvel.Entity ()
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types ()
import Marvel.Ability (HasAbilities)
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Id ()
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Query ()
import Marvel.Trait (Trait (Psionic))
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted ()

newtype NextEvolutionCampaignMinion = NextEvolutionCampaignMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40198 :: MinionCard NextEvolutionCampaignMinion
ne40198 = minion NextEvolutionCampaignMinion Cards.ne40198 (Sch 1) (Atk 1) (HP 3)

ne40199 :: MinionCard NextEvolutionCampaignMinion
ne40199 = minion NextEvolutionCampaignMinion Cards.ne40199 (Sch 1) (Atk 1) (HP 1)

ne40200 :: MinionCard NextEvolutionCampaignMinion
ne40200 = minion NextEvolutionCampaignMinion Cards.ne40200 (Sch 1) (Atk 1) (HP 3)

ne40201 :: MinionCard NextEvolutionCampaignMinion
ne40201 = minion NextEvolutionCampaignMinion Cards.ne40201 (Sch 1) (Atk 1) (HP 3)

instance RunMessage NextEvolutionCampaignMinion where
  runMessage msg e@(NextEvolutionCampaignMinion a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      case toCardCode a of
        "40198" -> do
          hand <- projectMap PlayerIdentityHand unHand you
          let costs = [fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card | card <- hand, cdCardType (getCardDef card) == EventType]
          damageIdentity you a (toDamage (maximum $ 0 : costs) FromAbility)
        "40200" -> do
          upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
          chooseAndDiscardControlled you (map UpgradeRef upgrades)
        "40201" -> returnHighestSupport you
        _ -> pure ()
      pure e
    MinionMessage mid MinionDefeated | mid == minionId a, toCardCode a == "40199" -> do
      -- The engine keeps Malice as an attached MINION, rather than replacing her
      -- with an attachment. Her separate card and damage remain targetable.
      candidates <- selectList (NotAlly $ AllyWithTrait Psionic)
      defs <- for candidates \aid -> (aid,) <$> getsGame (`abilityCardDef` AllyRef aid)
      let costed = [(aid, max 0 $ fromMaybe 0 $ cdCost =<< def) | (aid,def) <- defs]
          largest = maximum $ 0 : map snd costed
          highest = [aid | (aid,cost) <- costed, cost == largest]
      if null highest
        then NextEvolutionCampaignMinion <$> runMessage msg a
        else do
          first <- selectJust FirstPlayer
          chooseOne first [targeting (AllyRef aid) (attachDefeatedMinionToAlly (minionId a) aid) | aid <- highest]
          pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "40198" -> do
          hand <- filter ((== EventType) . cdCardType . getCardDef) <$> projectMap PlayerIdentityHand unHand you
          unless (null hand) $ chooseOne you [targeting card (discardCard card) | card <- hand]
        "40201" -> returnHighestSupport you
        _ -> pure ()
      pure e
    _ -> NextEvolutionCampaignMinion <$> runMessage msg a

returnHighestSupport :: ReverseQueue m => IdentityId -> m ()
returnHighestSupport you = do
  cards <- highestCostControlled you False True
  unless (null cards) $ chooseOne you [targeting target (returnToHand target) | target <- cards]
