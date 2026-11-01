module Marvel.Upgrade.Upgrades.SmCampaign where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Keyword (Keyword (Overkill, Piercing, Retaliate, Steady, Stalwart))
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Matchers qualified as Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

data SmTechMeta = SmTechMeta {impactProtected :: Bool, propulsionBoosted :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SmCampaignUpgrade = SmCampaignUpgrade (Attrs Upgrade `With` SmTechMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance IsUpgrade SmCampaignUpgrade where
  toUpgradeAttrs (SmCampaignUpgrade (attrs `With` _)) = attrs

techCode :: Attrs Upgrade -> CardCode
techCode = toBaseCardCode . toCardCode

enhanced :: Attrs Upgrade -> Bool
enhanced attrs = toCardCode attrs `elem` ["27182b", "27183b", "27184b", "27185b", "27186b", "27187b", "27188b", "27189b"]

smCompactDartsA :: UpgradeCard SmCampaignUpgrade
smCompactDartsA = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smCompactDartsA

smCompactDartsB :: UpgradeCard SmCampaignUpgrade
smCompactDartsB = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smCompactDartsB

smImpactDampeningSuitA :: UpgradeCard SmCampaignUpgrade
smImpactDampeningSuitA = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smImpactDampeningSuitA

smImpactDampeningSuitB :: UpgradeCard SmCampaignUpgrade
smImpactDampeningSuitB = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smImpactDampeningSuitB

smLaserGogglesA :: UpgradeCard SmCampaignUpgrade
smLaserGogglesA = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smLaserGogglesA

smLaserGogglesB :: UpgradeCard SmCampaignUpgrade
smLaserGogglesB = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smLaserGogglesB

smPropulsionGauntletA :: UpgradeCard SmCampaignUpgrade
smPropulsionGauntletA = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smPropulsionGauntletA

smPropulsionGauntletB :: UpgradeCard SmCampaignUpgrade
smPropulsionGauntletB = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smPropulsionGauntletB

smRetinalDisplayA :: UpgradeCard SmCampaignUpgrade
smRetinalDisplayA = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smRetinalDisplayA

smRetinalDisplayB :: UpgradeCard SmCampaignUpgrade
smRetinalDisplayB = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smRetinalDisplayB

smShockKnucklesA :: UpgradeCard SmCampaignUpgrade
smShockKnucklesA = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smShockKnucklesA

smShockKnucklesB :: UpgradeCard SmCampaignUpgrade
smShockKnucklesB = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smShockKnucklesB

smWaveBracersA :: UpgradeCard SmCampaignUpgrade
smWaveBracersA = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smWaveBracersA

smWaveBracersB :: UpgradeCard SmCampaignUpgrade
smWaveBracersB = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smWaveBracersB

smWristNavigatorA :: UpgradeCard SmCampaignUpgrade
smWristNavigatorA = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smWristNavigatorA

smWristNavigatorB :: UpgradeCard SmCampaignUpgrade
smWristNavigatorB = upgrade (SmCampaignUpgrade . (`With` SmTechMeta False False)) Cards.smWristNavigatorB

instance HasAbilities SmCampaignUpgrade where
  getAbilities a@(SmCampaignUpgrade (attrs `With` _)) = case techCode attrs of
    "27182" ->
      [ limitedWindowAbility a 1 (W.IdentityAttacked W.After You AnyEnemy) HeroResponse
          (OwnsThis <> EnemyExists DamageableEnemy) UseCost (runAbility a 1)
      , limitedAbility a 2 (PerRound 1) AlterEgoAction OwnsThis (ResourceCost Nothing) (runAbility a 2) ]
    "27183" | enhanced attrs ->
      [limitedWindowAbility a 1 (W.WouldTakeDamage You FromAttack AnyValue) HeroInterrupt OwnsThis DiscardTopOfDeckCost (runAbility a 1)]
    "27183" ->
      [limitedWindowAbility a 1 (W.PhaseBegins W.When VillainPhase) HeroInterrupt OwnsThis (ResourceCost Nothing) (runAbility a 1)]
    "27185" ->
      [ability a 1 HeroAction (OwnsThis <> SelfMatches Matchers.ExhaustedIdentity) (ExhaustCost <> IndirectDamageCost 2) (runAbility a 1)]
    "27187" ->
      [limitedWindowAbility a 1 (W.MakesBasicAttackAgainst W.After You AnyEnemy) HeroResponse OwnsThis NoCost (runAbility a 1)]
    "27189" ->
      [windowAbility a 1 (W.WindowOneOf [W.MinionEntersPlay W.After AnyMinion, W.SideSchemeEntersPlay W.After AnySideScheme])
        ForcedResponse NoCost (runAbility a 1)]
        <> [limitedWindowAbility a 2 window Interrupt OwnsThis NoCost (runAbility a 2)
          | window <- maybe [] pure $ attachedWindow attrs]
    _ -> []
   where
    attachedWindow attrs = case upgradeAttachedEnemy attrs of
      Just (EnemyMinionId mid) -> Just $ W.MinionDefeated W.When $ MinionWithId mid
      _ -> W.SideSchemeDefeated W.When . SideSchemeWithId <$> upgradeAttachedSideScheme attrs

instance HasModifiersFor SmCampaignUpgrade where
  getModifiersFor _ (IdentityRef who) (SmCampaignUpgrade (attrs `With` meta)) | who == upgradeController attrs = do
    hero <- selectAny (IdentityWithId who <> HeroIdentity)
    let extra = if enhanced attrs then 2 else 1
        permanent = [HitPointModifier (if enhanced attrs then 3 else 2) | techCode attrs == "27183"]
        inHero = case techCode attrs of
          "27183" -> [AttackDamageReduction 1 | impactProtected meta]
          "27184" -> [ThwartPenalty 1, AttackModifier extra, KeywordModifier Overkill] <> [KeywordModifier Piercing | enhanced attrs]
          "27185" -> if propulsionBoosted meta then [ThwartModifier 1, AttackModifier 1, DefenseModifier 1] else []
          "27186" -> [ThwartModifier extra, BasicThwartOnlyMostThreat, BasicThwartIgnoresCrisis]
            <> [BasicThwartIgnoresPatrol | enhanced attrs]
          "27187" -> [AttackModifier 1 | enhanced attrs]
          "27188" -> [AttackPenalty 1, DefenseModifier extra, KeywordModifier (Retaliate 1), KeywordModifier (if enhanced attrs then Stalwart else Steady)]
          _ -> []
    pure $ permanent <> [modifier | hero, modifier <- inHero]
  getModifiersFor _ _ _ = pure []

instance RunMessage SmCampaignUpgrade where
  runMessage msg u@(SmCampaignUpgrade (attrs `With` meta)) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "enhance" _ ->
      pure $ SmCampaignUpgrade (attrs {upgradeCardDef = improved attrs} `With` meta)
    RanAbility you (isTarget attrs -> True) 1 _ _ | techCode attrs == "27182" -> do
      cardSpecific attrs "darts" (you, if enhanced attrs then 2 else 1 :: Natural, [] :: [EnemyId])
      pure u
    RanAbility _ (isTarget attrs -> True) 2 _ _ | techCode attrs == "27182" -> do
      upgradeStep (upgradeId attrs) (AddUpgradeUses 3)
      pure u
    CardSpecific (isTarget attrs -> True) "darts" (fromJSON -> Success (you, remaining :: Natural, chosen :: [EnemyId])) -> do
      enemies <- filter (`notElem` chosen) <$> selectList DamageableEnemy
      when (remaining > 0 && notNull enemies) $ chooseOne you $
        [labeled "$label.done" (pure ()) | enhanced attrs]
        <> [targeting (enemyRef enemy) do
            dealDamage enemy attrs (toDamage 1 FromAbility)
            cardSpecific attrs "darts" (you, remaining - 1, enemy : chosen)
          | enemy <- enemies]
      pure u
    RanAbility you (isTarget attrs -> True) 1 _ _ | techCode attrs == "27183" ->
      if enhanced attrs then u <$ preventDamage you 1
      else pure $ SmCampaignUpgrade (attrs `With` meta {impactProtected = True})
    RanAbility you (isTarget attrs -> True) 1 _ _ | techCode attrs == "27185" -> do
      ready you
      pure $ SmCampaignUpgrade (attrs `With` meta {propulsionBoosted = enhanced attrs || propulsionBoosted meta})
    RanAbility _ (isTarget attrs -> True) 1 windows _ | techCode attrs == "27187" -> do
      for_ [enemy | W.MadeBasicAttackAgainst _ enemy <- windows] $ \enemy ->
        discardTopOfEncounterDeckThen 1 (countBoostIcons attrs enemy)
      pure u
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success enemy) (sum . map snd -> n) | techCode attrs == "27187" -> do
      when (n <= if enhanced attrs then 1 else 0) $ stun (enemy :: EnemyId) attrs
      pure u
    RanAbility _ (isTarget attrs -> True) 1 windows _ | techCode attrs == "27189" -> do
      detachUpgrade (upgradeId attrs)
      for_ windows $ \case
        W.MinionEnteredPlay mid -> upgradeStep (upgradeId attrs) (UpgradeAttachedToEnemy $ EnemyMinionId mid)
        W.SideSchemeEnteredPlay sid -> upgradeStep (upgradeId attrs) (UpgradeAttachedToSideScheme sid)
        _ -> pure ()
      pure u
    RanAbility you (isTarget attrs -> True) 2 _ _ | techCode attrs == "27189" -> do
      drawCards you (if enhanced attrs then 2 else 1)
      when (enhanced attrs) $ discardFromHand you 1
      detachUpgrade (upgradeId attrs)
      pure u
    EndPhase _ -> pure $ SmCampaignUpgrade (attrs `With` SmTechMeta False False)
    _ -> (\a -> SmCampaignUpgrade (a `With` meta)) <$> runMessage msg attrs
   where
    improved attrs = case techCode attrs of
      "27182" -> Cards.smCompactDartsB
      "27183" -> Cards.smImpactDampeningSuitB
      "27184" -> Cards.smLaserGogglesB
      "27185" -> Cards.smPropulsionGauntletB
      "27186" -> Cards.smRetinalDisplayB
      "27187" -> Cards.smShockKnucklesB
      "27188" -> Cards.smWaveBracersB
      "27189" -> Cards.smWristNavigatorB
      _ -> upgradeCardDef attrs
