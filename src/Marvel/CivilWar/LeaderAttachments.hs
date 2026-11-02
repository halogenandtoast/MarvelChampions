module Marvel.CivilWar.LeaderAttachments where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.EncounterSet qualified as E
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Window qualified as W

newtype LeaderAttachment = LeaderAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

leaderAttachmentDefinitions :: [CardDef]
leaderAttachmentDefinitions = [C.cw56099,C.cw56144,C.cw56145,C.cw57008,C.cw57047,C.cw57048,C.cw57050]

leaderAttachments :: [AttachmentCard LeaderAttachment]
leaderAttachments = map (attachment LeaderAttachment) leaderAttachmentDefinitions

instance HasModifiersFor LeaderAttachment where
  getModifiersFor _ target (LeaderAttachment a)
    | Just target == (enemyRef <$> attachmentEnemy a) = pure $ case toCardCode a of
        "56145" -> [AttackModifier 2]
        "57048" -> [AttackModifier 1,KeywordModifier $ Retaliate 1]
        "57050" -> []
        _ -> [SchemeModifier 1,KeywordModifier Stalwart]
  getModifiersFor _ _ _ = pure []

massFormMatches :: EnemyId -> CardDef -> AttachmentMatcher
massFormMatches enemy def = AttachmentMatches [AttachmentOnEnemy enemy,AttachmentIs def]

instance HasAbilities LeaderAttachment where
  getAbilities (LeaderAttachment a) = case attachmentEnemy a of
    Nothing -> []
    Just enemy -> case toCardCode a of
      "56145" -> [windowAbility a 2 (W.EnemyAttackedAnyTarget W.When $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 2)]
      "57008" -> [windowAbility a 2 (W.VillainSchemed W.After $ VillainWithId vid) ForcedResponse NoCost (runAbility a 2) | EnemyVillainId vid <- [enemy]]
      "57050" ->
        [limitedWindowAbility a idx window ForcedInterrupt (AttachmentExists $ massFormMatches enemy form) NoCost (runAbility a idx)
        | (idx,window,form) <- [(2,W.EnemyWouldScheme (EnemyWithId enemy) AnyIdentity,C.cw57046a),(3,W.EnemyAttackedAnyTarget W.Would $ EnemyWithId enemy,C.cw57046b)]]
      _ -> [windowAbility a 1 (W.MakesBasicAttackAgainst W.After You $ EnemyWithId enemy) HeroResponse cost (runAbility a 1)]
        where
          cost = case toCardCode a of
            "57047" -> MultiResourceCost [Just Energy,Just Physical]
            "57048" -> MultiResourceCost [Just Energy,Just Mental]
            _ -> SameResourcesCost 3

instance RunMessage LeaderAttachment where
  runMessage msg e@(LeaderAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      let family = case toCardCode a of
            "56099" -> E.CwCaptainMarvel
            "56144" -> E.CwCaptainAmerica
            "56145" -> E.CwCaptainAmerica
            "57008" -> E.CwSheHulk
            _ -> E.CwVision
      asYou you (leaderOfFamily family) >>= \case
        Nothing -> removeFromPlay a
        Just vid -> attachTo aid vid
      pure e
    RanAbility _ (isTarget a -> True) idx _ _ -> do
      case (toCardCode a,attachmentEnemy a) of
        ("56145",Just enemy) -> do
          attackGains enemy Overkill
          afterMatching (\case VillainMessage vid VillainEndAttack -> enemy == EnemyVillainId vid; _ -> False) (removeFromPlay a)
        (_,Just enemy) -> do
          when (toCardCode a `elem` ["57047","57048","57050"]) $ flipLeaderMassForm enemy
          when (idx == 1 || toCardCode a `elem` ["57008","57050"]) $ removeFromPlay a
        _ -> pure ()
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a /= "57050" -> do
      revealBoost a =<< getBoostedPlayer enemy
      pure e
    _ -> LeaderAttachment <$> runMessage msg a
