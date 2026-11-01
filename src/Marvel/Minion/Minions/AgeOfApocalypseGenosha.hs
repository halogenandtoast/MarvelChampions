module Marvel.Minion.Minions.AgeOfApocalypseGenosha where

import Marvel.Prelude
import Marvel.Trait ()
import Marvel.Keyword ()
import Marvel.Damage ()
import Marvel.Ability
import Marvel.AgeOfApocalypse.Settings
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Hp
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Attachments

newtype AoaGenoshaMinion = AoaGenoshaMinion (Attrs Minion `With` Maybe IdentityId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion AoaGenoshaMinion where
  toMinionAttrs (AoaGenoshaMinion (attrs `With` _)) = attrs
  overMinionAttrs f (AoaGenoshaMinion (attrs `With` actor)) = AoaGenoshaMinion (f attrs `With` actor)

magistrate :: MinionCard AoaGenoshaMinion
magistrate = minion (AoaGenoshaMinion . (`With` Nothing)) Cards.magistrate (Sch 2) (Atk 2) (HP 3)

armoredUnibike :: MinionCard AoaGenoshaMinion
armoredUnibike = minion (AoaGenoshaMinion . (`With` Nothing)) Cards.armoredUnibike (Sch 1) (Atk 2) (HP 4)

genoshanMech :: MinionCard AoaGenoshaMinion
genoshanMech = minion (AoaGenoshaMinion . (`With` Nothing)) Cards.genoshanMech (Sch 2) (Atk 3) (HP 5)

instance HasModifiersFor AoaGenoshaMinion

instance HasAbilities AoaGenoshaMinion where
  getAbilities a@(AoaGenoshaMinion (attrs `With` _)) = case toCardCode attrs of
    "45135" -> [limitedWindowAbility a 1 (W.EnemyAttacked W.After own AnyIdentity) ForcedResponse NoCriteria NoCost (runAbility a 1)]
    "45136" -> [limitedWindowAbility a 2 (W.AllyDefeatedByAttackOf W.After own) ForcedResponse NoCriteria NoCost (runAbility a 2)]
    _ -> []
   where own = EnemyWithId $ EnemyMinionId $ minionId attrs

instance RunMessage AoaGenoshaMinion where
  runMessage msg e@(AoaGenoshaMinion (attrs `With` actor)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion _) | mid == minionId attrs, toCardCode attrs == "45135" -> do
      escaped <- selectList (AttachmentIs Attachments.escapedMutant)
      hosts <- catMaybes <$> traverse getAttachmentIdentity escaped
      for_ (headMay hosts) (redirectPendingMinionEngagement mid)
      pure e
    MinionMessage mid (MinionDefeatedWithExcess source _ _) | mid == minionId attrs -> do
      who <- getSourceController source
      updated <- runMessage msg attrs
      pure $ AoaGenoshaMinion (updated `With` who)
    MinionMessage mid MinionDefeated | mid == minionId attrs, toCardCode attrs == "45134" -> do
      who <- maybe getActivePlayerId pure actor
      findEscapedMutant who
      AoaGenoshaMinion . (`With` actor) <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) n windows _ -> do
      players <- case n of
        1 -> pure ([you | W.EnemyAttack _ you <- windows] <> [you | W.EnemyAttackAlly _ _ you <- windows])
        _ -> pure . pure =<< getBoostedPlayer (EnemyMinionId $ minionId attrs)
      for_ players \you -> do
        resolveSettingSpecial you
        when (n == 2) $ resolveSettingSpecial you
      pure e
    Boost (RevealedAsBoost target enemy) | target == toRef attrs, toCardCode attrs == "45134" -> do
      you <- getBoostedPlayer enemy
      escaped <- selectList (AttachmentIs Attachments.escapedMutant)
      hosts <- catMaybes <$> traverse getAttachmentIdentity escaped
      when (you `elem` hosts) $ addEnemyBoost enemy 3
      pure e
    _ -> AoaGenoshaMinion . (`With` actor) <$> runMessage msg attrs

findEscapedMutant :: ReverseQueue m => IdentityId -> m ()
findEscapedMutant you = do
  escaped <- selectList (AttachmentIs Attachments.escapedMutant)
  case escaped of
    aid : _ -> attachToIdentity aid you
    [] -> do
      cards <- findEncounterCards (CardIs Attachments.escapedMutant)
      for_ (headMay cards) \card -> do
        removeFromGame (CardIdRef $ toCardId card)
        putEncounterCardIntoPlay you card

findAndRevealEscapedMutant :: ReverseQueue m => IdentityId -> m ()
findAndRevealEscapedMutant you = do
  cards <- findEncounterCards (CardIs Attachments.escapedMutant)
  for_ (headMay cards) \card -> do
    escaped <- selectList (AttachmentIs Attachments.escapedMutant)
    unless (notNull escaped) $ removeFromGame (CardIdRef $ toCardId card)
    revealEncounterCard you card
