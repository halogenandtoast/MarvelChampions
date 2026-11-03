module Marvel.CivilWar.HellsKitchen where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T

newtype Daredevil = Daredevil (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

daredevil :: MinionCard Daredevil
daredevil = minion Daredevil C.cw56189 (Sch 2) (Atk 2) (HP 3)

instance RunMessage Daredevil where
  runMessage msg e@(Daredevil a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      choices <- exhaustionChoices you
      chooseForEnemyTeam you a choices
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      choices <- exhaustionChoices you
      unless (null choices) $ chooseOne you choices
      pure e
    _ -> Daredevil <$> runMessage msg a
   where
    exhaustionChoices you = do
      identities <- asYou you $ selectList (IdentityWithId you <> UnexhaustedIdentity)
      allies <- asYou you $ selectList (AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
      pure $ [targeting (characterRef $ IdentityCharacter iid) (exhaust iid) | iid <- identities]
        <> [targeting (characterRef $ AllyCharacter aid) (exhaust aid) | aid <- allies]

newtype IronFist = IronFist (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

ironFist :: MinionCard IronFist
ironFist = minion IronFist C.cw56190 (Sch 1) (Atk 3) (HP 4)

instance RunMessage IronFist where
  runMessage msg e@(IronFist a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      friendlyLeaderFor you >>= \case
        Just vid -> do
          first <- asYou you $ selectJust FirstPlayer
          minionAttacksEnemyAs mid first vid
        Nothing -> minionActivatesAgainst mid you
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      attacker <- enemyLeaderFor you
      defender <- friendlyLeaderFor you
      for_ [(vid,leader) | Just vid <- [attacker],Just leader <- [defender]] $ \(vid,leader) -> do
        first <- firstPlayerAtLeader vid
        villainAttacksCharacterWithoutBoost vid first leader
      pure e
    _ -> IronFist <$> runMessage msg a

newtype ResistanceFighter = ResistanceFighter (Attrs Attachment)
  deriving anyclass (IsAttachment,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

resistanceFighter :: AttachmentCard ResistanceFighter
resistanceFighter = attachment ResistanceFighter C.cw56191

instance HasModifiersFor ResistanceFighter where
  getModifiersFor _ target (ResistanceFighter a)
    | Just (EnemyMinionId mid) <- attachmentEnemy a, target == enemyRef (EnemyMinionId mid) =
        pure [HitPointModifier 4,KeywordModifier Stalwart]
  getModifiersFor _ _ _ = pure []

instance RunMessage ResistanceFighter where
  runMessage msg e@(ResistanceFighter a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      candidates <- asYou you $ selectList AnyMinion
      minions <- filterM eligible candidates
      if null minions then surge you >> removeFromPlay a
      else chooseForEnemyTeam you a [targeting (enemyRef $ EnemyMinionId mid) (attachTo aid mid) | mid <- minions]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      candidates <- asYou you $ selectList (MinionEngagedWith $ IdentityWithId you)
      minions <- filterM eligible candidates
      unless (null minions) $ chooseOne you
        [targeting (enemyRef $ EnemyMinionId mid) (do
          card <- getBoostCard (toTarget a)
          for_ card $ \c -> consumeBoostCard c >> attachEncounterCard c mid) | mid <- minions]
      pure e
    _ -> ResistanceFighter <$> runMessage msg a
   where
    eligible mid = do
      modifiers <- getModifiers (enemyRef $ EnemyMinionId mid)
      pure $ all (`notElem` modifiers) [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]

newtype DefendHellsKitchen = DefendHellsKitchen (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

defendHellsKitchen :: SideSchemeCard DefendHellsKitchen
defendHellsKitchen = sideScheme DefendHellsKitchen C.cw56193 (Static 3)

instance HasModifiersFor DefendHellsKitchen where
  getModifiersFor _ (SideSchemeRef sid) (DefendHellsKitchen a) | sid == sideSchemeId a = do
    defender <- selectAny $ MinionWithTrait T.Defender
    pure [ThreatCannotBeRemoved | defender]
  getModifiersFor _ _ _ = pure []

instance RunMessage DefendHellsKitchen where
  runMessage msg (DefendHellsKitchen a) = runQueueT $ DefendHellsKitchen <$> runMessage msg a
