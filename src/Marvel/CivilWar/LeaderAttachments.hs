module Marvel.CivilWar.LeaderAttachments where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
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
leaderAttachmentDefinitions = [C.cw56099,C.cw56100,C.cw56144,C.cw56145,C.cw56175,C.cw57008,C.cw57047,C.cw57048,C.cw57049,C.cw57050]

leaderAttachments :: [AttachmentCard LeaderAttachment]
leaderAttachments = map (attachment LeaderAttachment) leaderAttachmentDefinitions

instance HasModifiersFor LeaderAttachment where
  getModifiersFor _ target (LeaderAttachment a)
    | Just target == (enemyRef <$> attachmentEnemy a) = pure $ case toCardCode a of
        "56145" -> [AttackModifier 2]
        "56100" -> [AttackModifier 1]
        "56175" -> [SchemeModifier 1]
        "57048" -> [AttackModifier 1,KeywordModifier $ Retaliate 1]
        "57050" -> []
        "57049" -> []
        _ -> [SchemeModifier 1,KeywordModifier Stalwart]
  getModifiersFor _ _ _ = pure []

massFormMatches :: EnemyId -> CardDef -> AttachmentMatcher
massFormMatches enemy def = AttachmentMatches [AttachmentOnEnemy enemy,AttachmentIs def]

instance HasAbilities LeaderAttachment where
  getAbilities (LeaderAttachment a) = case attachmentEnemy a of
    Nothing -> []
    Just enemy -> case toCardCode a of
      "56100" -> [windowAbility a 2 (W.VillainWouldTakeDamageAtLeastFrom (VillainWithId vid) DamageFromAnAttack 4) ForcedInterrupt NoCost (runAbility a 2) | EnemyVillainId vid <- [enemy]]
      "56175" -> [windowAbility a 2 (W.VillainWouldGainStatus (VillainWithId vid) [StunnedStatus,ConfusedStatus]) ForcedInterrupt NoCost (runAbility a 2) | EnemyVillainId vid <- [enemy]]
      "57049" -> [limitedWindowAbility a 2 (W.WindowOneOf [W.IdentityAttacked W.When AnyIdentity (EnemyWithId enemy),W.AllyAttacked W.When AnyAlly (EnemyWithId enemy),W.EnemyWasAttacked W.When (EnemyWithId enemy)]) ForcedInterrupt
        (AttachmentExists $ massFormMatches enemy C.cw57046a) NoCost (runAbility a 2)]
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
            "56100" -> E.CwCaptainMarvel
            "56144" -> E.CwCaptainAmerica
            "56145" -> E.CwCaptainAmerica
            "57008" -> E.CwSheHulk
            "56175" -> E.CwSpiderWoman
            _ -> E.CwVision
      asYou you (leaderOfFamily family) >>= \case
        Nothing -> removeFromPlay a
        Just vid -> attachTo aid vid
      pure e
    RanAbility _ (isTarget a -> True) idx windows _ -> do
      case (toCardCode a,attachmentEnemy a) of
        ("56100",Just enemy) -> do
          for_ [damage | W.VillainDamageFrom _ _ damage <- windows] $ \damage ->
            preventEnemyDamage enemy (subtractNatural 3 $ damageAmount damage)
          removeFromPlay a
        ("56175",Just enemy) -> do
          insteadOf (\case CardSpecific target "gainVillainStatus" _ -> target == enemyRef enemy; _ -> False) $ do
            removeFromPlay a
            giveStatus (toCharacter enemy) ToughStatus
        ("57049",Just _) -> do
          let attackers = [IdentityCharacter who | W.IdentityAttack who _ <- windows]
                <> [AllyCharacter aid | W.AllyAttack aid _ <- windows]
                <> [toCharacter attacker | W.EnemyAttackOnEnemy attacker _ <- windows]
          for_ (take 1 attackers) $ \attacker -> do
            case attacker of
              IdentityCharacter who -> do
                source <- fromMaybe (IdentityRef who) <$> currentPlayerAttackSource who
                preventAllDamageFromAttack source
              AllyCharacter aid -> preventAllDamageFromAttack $ AllyRef aid
              VillainCharacter vid -> preventAttackDamage vid
              MinionCharacter mid -> preventAttackDamage mid
              AttachmentCharacter _ -> pure ()
            giveStatus attacker StunnedStatus
          removeFromPlay a
        ("56145",Just enemy) -> do
          attackGains enemy Overkill
          afterMatching (\case VillainMessage vid VillainEndAttack -> enemy == EnemyVillainId vid; _ -> False) (removeFromPlay a)
        (_,Just enemy) -> do
          when (toCardCode a `elem` ["57047","57048","57050"]) $ flipLeaderMassForm enemy
          when (idx == 1 || toCardCode a `elem` ["57008","57050"]) $ removeFromPlay a
        _ -> pure ()
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a `notElem` ["57049","57050"] -> do
      revealBoost a =<< getBoostedPlayer enemy
      pure e
    _ -> LeaderAttachment <$> runMessage msg a
