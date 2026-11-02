module Marvel.CampaignPlayers.Obligations where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CampaignPlayers.EncounterCards qualified as Cards
import Marvel.Criteria
import Marvel.Cost
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Types
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

cn56023, cn56054, cn60032, cn60060 :: ObligationCard CampaignObligation
cn56023 = obligation makeObligation Cards.enc56023
cn56054 = obligation makeObligation Cards.enc56054
cn60032 = obligation makeObligation Cards.enc60032
cn60060 = obligation makeObligation Cards.enc60060

makeObligation :: Attrs Obligation -> CampaignObligation
makeObligation a = CampaignObligation (a `With` ObligationMeta 0 Nothing False)

data ObligationMeta = ObligationMeta
  { accumulatedThreat :: Natural
  , watchingMinions :: Maybe [MinionId]
  , minionActivated :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CampaignObligation = CampaignObligation (Attrs Obligation `With` ObligationMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance IsRef CampaignObligation where toRef (CampaignObligation (a `With` _)) = toRef a
instance HasCardCode CampaignObligation where toCardCode (CampaignObligation (a `With` _)) = toCardCode a
instance IsObligation CampaignObligation where
  toObligationAttrs (CampaignObligation (a `With` _)) = a
  obligationDisplayThreat (CampaignObligation (_ `With` meta)) = accumulatedThreat meta

instance HasModifiersFor CampaignObligation where
  getModifiersFor _ (MinionRef mid) (CampaignObligation (a `With` _)) | toCardCode a == "60060", Just you <- obligationOwner a = do
    kingpin <- getsGame $ maybe False (sameCard Cards.enc60061) . (`abilityCardDef` MinionRef mid)
    pure [CannotBeDamagedByIdentity you | kingpin]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CampaignObligation where
  getAbilities (CampaignObligation (a `With` _)) = case obligationOwner a of
    Nothing -> []
    Just you -> case toCardCode a of
      "60032" ->
        [limitedWindowAbility a 1 (W.PlayerCardEntersPlay W.After (IdentityWithId you) $ CardWithType UpgradeType <> CardWithTrait T.Sense) ForcedResponse OwnsThis NoCost $ runAbility a 1
        ,limitedWindowAbility a 2 (W.UsesRecoveryAt W.After $ IdentityWithId you) AlterEgoResponse OwnsThis NoCost $ runAbility a 2]
      "60060" -> [limitedWindowAbility a 3 (W.CharacterThwartsScheme W.After (SourceExtendsIdentity $ IdentityWithId you) AnyScheme) Response OwnsThis NoCost $ runAbility a 3]
      _ -> []

instance RunMessage CampaignObligation where
  runMessage msg e@(CampaignObligation (a `With` meta)) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> do
      let owned = a {obligationOwner = Just you}
      case toCardCode a of
        "60032" -> pure $ CampaignObligation (owned `With` meta)
        "60060" -> do
          searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Cards.enc60061)
          pure $ CampaignObligation (owned `With` meta)
        _ -> do
          hero <- identityMatches HeroIdentity you
          when hero $ chooseOne you
            [labeled "$label.doNotFlipToAlterEgo" $ pure (), labeled "$label.changeToB" $ changeToAlterEgo you]
          resolveObligation oid you
          pure $ CampaignObligation (owned `With` meta)
    ObligationMessage oid (ResolveObligation you) | oid == obligationId a -> do
      ready <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      chooseOrRunOne you $
        [labeled "$label.cnExhaustRemoveObligation" $ exhaust you >> removeFromGame a | ready]
        <> [labeled (if toCardCode a == "56023" then "$label.cnActivateMinions" else "$label.cnDiscardShapeshifts") $ cardSpecific a "consequence" you]
      pure e
    CardSpecific (isTarget a -> True) "consequence" (fromJSON -> Success you) -> case toCardCode a of
      "56023" -> do
        minions <- selectList $ MinionEngagedWith $ IdentityWithId you
        if null minions then e <$ surge you else do
          chooseOneAtATimePrompted you "$label.chooseMinionToActivate"
            [targeting (enemyRef $ EnemyMinionId mid) (minionActivatesAgainst mid you) | mid <- minions]
          cardSpecific a "checkActivations" you
          pure $ CampaignObligation (a `With` meta {watchingMinions = Just minions, minionActivated = False})
      "56054" -> do
        upgrades <- selectList $ UpgradeControlledBy (IdentityWithId you) <> UpgradeWithTrait T.Shapeshift
        traverse_ removeFromPlay upgrades
        pure e
      _ -> pure e
    CheckWindows windows | Just minions <- watchingMinions meta
      , any (`elem` minions) [mid | W.Window W.When (W.EnemyActivation (EnemyMinionId mid) _) <- windows] ->
        pure $ CampaignObligation (a `With` meta {minionActivated = True})
    CardSpecific (isTarget a -> True) "checkActivations" (fromJSON -> Success you) -> do
      unless (minionActivated meta) $ surge you
      pure $ CampaignObligation (a `With` meta {watchingMinions = Nothing, minionActivated = False})
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a, toCardCode a `elem` ["60032","60060"] -> pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ damageIdentity you a (toDamage 1 FromAbility)
    RanAbility _ (isTarget a -> True) 2 _ _ -> e <$ removeFromPlay a
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      let total = accumulatedThreat meta + sum [removed | W.ThwartRemovedThreat _ _ _ removed <- windows]
      when (total >= 4) $ removeFromGame a
      pure $ CampaignObligation (a `With` meta {accumulatedThreat = total})
    _ -> (\a' -> CampaignObligation (a' `With` meta)) <$> runMessage msg a
