{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.SideScheme.SideSchemes.AgentsOfShield where

import Marvel.Prelude
import Control.Monad (replicateM_)
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.AgentsOfShield.Board
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.AgentsOfShield.Helpers
import Marvel.Boost (boostCount)
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hood.Helpers (controlledCharacters)
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types (Field (MinionCardDef))
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.SideScheme.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype AosSideScheme = AosSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme AosSideScheme where
  sideSchemeAbilities = getAbilities

aos50074 :: SideSchemeCard AosSideScheme
aos50074 = sideScheme AosSideScheme Cards.aos50074 (Static 3)

aos50075 :: SideSchemeCard AosSideScheme
aos50075 = sideScheme AosSideScheme Cards.aos50075 (Static 2)

aos50081 :: SideSchemeCard AosSideScheme
aos50081 = sideScheme AosSideScheme Cards.aos50081 (PerPlayer 2)

aos50085 :: SideSchemeCard AosSideScheme
aos50085 = sideScheme AosSideScheme Cards.aos50085 (Static 2)

aos50095 :: SideSchemeCard AosSideScheme
aos50095 = sideScheme AosSideScheme Cards.aos50095 (PerPlayer 3)

aos50101 :: SideSchemeCard AosSideScheme
aos50101 = sideScheme AosSideScheme Cards.aos50101 (Static 6)

aos50121 :: SideSchemeCard AosSideScheme
aos50121 = sideScheme AosSideScheme Cards.aos50121 (PerPlayer 3)

aos50122 :: SideSchemeCard AosSideScheme
aos50122 = sideScheme AosSideScheme Cards.aos50122 (Static 3)

aos50127 :: SideSchemeCard AosSideScheme
aos50127 = sideScheme AosSideScheme Cards.aos50127 (PerPlayer 3)

aos50135 :: SideSchemeCard AosSideScheme
aos50135 = sideScheme AosSideScheme Cards.aos50135 (PerPlayer 2)

aos50136 :: SideSchemeCard AosSideScheme
aos50136 = sideScheme AosSideScheme Cards.aos50136 (PerPlayer 3)

aos50140 :: SideSchemeCard AosSideScheme
aos50140 = sideScheme AosSideScheme Cards.aos50140 (PerPlayer 3)

aos50146 :: SideSchemeCard AosSideScheme
aos50146 = sideScheme AosSideScheme Cards.aos50146 (PerPlayer 3)

aos50150 :: SideSchemeCard AosSideScheme
aos50150 = sideScheme AosSideScheme Cards.aos50150 (Static 4)

aos50154 :: SideSchemeCard AosNuclearReaction
aos50154 = sideScheme (AosNuclearReaction . (`With` False)) Cards.aos50154 (Static 5)

aos50159 :: SideSchemeCard AosSideScheme
aos50159 = sideScheme AosSideScheme Cards.aos50159 (Static 3)

aos50162 :: SideSchemeCard AosSideScheme
aos50162 = sideScheme AosSideScheme Cards.aos50162 (Static 5)

aos50173 :: SideSchemeCard AosSideScheme
aos50173 = sideScheme AosSideScheme Cards.aos50173 (Static 3)

aos50174 :: SideSchemeCard AosSideScheme
aos50174 = sideScheme AosSideScheme Cards.aos50174 (Static 3)

aos50180 :: SideSchemeCard AosSideScheme
aos50180 = sideScheme AosSideScheme Cards.aos50180 (Static 2)

instance HasModifiersFor AosSideScheme where
  getModifiersFor _ target (AosSideScheme a) = case toCardCode a of
    "50075" -> do
      def <- getsGame (`abilityCardDef` target)
      pure [KeywordModifier (Incite 1) | target /= toRef a, isJust (def >>= cdEncounterSet)]
    "50085" -> do
      affected <- case target of MinionRef mid -> minionMatches (MinionWithTrait Trait.Aim) mid; _ -> pure False
      pure [AccelerationIconModifier 1 | affected]
    "50101" -> pure [KeywordModifier Toughness | MinionRef _ <- [target]]
    "50121" -> do
      affected <- case target of VillainRef vid -> enemyMatches (EnemyWithTitle "M.O.D.O.K.") (EnemyVillainId vid); _ -> pure False
      pure [CannotTakeDamage | affected]
    "50140" -> do
      affected <- case target of
        IdentityRef you -> identityMatches (IdentityWithTrait Trait.Aerial) you
        AllyRef aid -> allyMatches (AllyWithTrait Trait.Aerial) aid
        MinionRef mid -> minionMatches (MinionWithTrait Trait.Aerial) mid
        VillainRef vid -> villainMatches (VillainWithTrait Trait.Aerial) vid
        _ -> pure False
      pure [AttackModifier 1 | affected]
    "50159" -> do
      aerial <- case target of
        IdentityRef you -> identityMatches (IdentityWithTrait Trait.Aerial) you
        AllyRef aid -> allyMatches (AllyWithTrait Trait.Aerial) aid
        MinionRef mid -> minionMatches (MinionWithTrait Trait.Aerial) mid
        VillainRef vid -> villainMatches (VillainWithTrait Trait.Aerial) vid
        _ -> pure False
      pure [AttackDamageReductionUnlessAerialOrRanged 2 | aerial]
    "50173" -> pure [AllyBasicPowerAdditionalCost (ResourceCost Nothing) | AllyRef _ <- [target]]
    "50174" -> pure [AdditionalReadyCost (ResourceCost Nothing) | SupportRef _ <- [target]]
    "50180" -> pure [AdditionalResourceCost (CardWithTrait Trait.Shield) 1 | IdentityRef _ <- [target]]
    _ -> pure []

instance HasAbilities AosSideScheme where
  getAbilities (AosSideScheme a) = case toCardCode a of
    "50081" -> [windowAbility a 1 (W.AllyLeavesPlay W.When AnyIdentity AnyCard) ForcedInterrupt NoCost (runAbility a 1)]
    "50101" -> [limitedWindowAbility a 5 (W.EncounterCardRevealedBy W.When You (EncounterCardMatching $ CardWithType MinionType <> NotCardMatching (CardWithTrait Trait.Elite))) HeroInterrupt CanCancelRevealedEncounterCard (MultiResourceCost $ replicate 3 Nothing) (runAbility a 5)]
    "50146" -> [windowAbility a 2 (W.EnemyWouldBeDealtDamage AnyEnemy) ForcedInterrupt NoCost (runAbility a 2)]
    "50154" -> [windowAbility a 3 (W.EnemyDealtDamage (EnemyIs Cards.aos50152) AnyDamage) ForcedResponse NoCost (runAbility a 3)]
    "50162" -> [windowAbility a 4 (W.EngagesMinion W.When AnyIdentity AnyMinion) ForcedInterrupt NoCost (runAbility a 4)]
    _ -> []

instance RunMessage AosSideScheme where
  runMessage msg e@(AosSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      case toCardCode a of
        "50095" -> whenM highAlert $ placeSideSchemeAccelerationToken sid
        "50101" -> selectList AnyEnemy >>= traverse_ (`giveStatus` ToughStatus)
        "50121" -> do
          allies <- selectList (AllyWithTrait Trait.Rescued)
          first <- selectJust FirstPlayer
          chooseEffect first [targeting aid (cardSpecific GameRef "aosHostageAlly" (aid,sid)) | aid <- allies]
        "50127" -> do
          n <- aimVictory
          replicateM_ n $ placeSideSchemeAccelerationToken sid
        "50135" -> rotateMinions
        "50136" -> rotateMinions
        "50150" -> do
          n <- countCardsInPlayWithTrait Trait.Preparation
          placeThreatOn a sid n
        "50180" -> do
          n <- countCardsInPlayWithTrait Trait.Shield
          placeThreatOn a sid n
        _ -> pure ()
      pure e
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      case toCardCode a of
        "50081" -> for_ (sideSchemeHeldCards a) $ \c -> for_ (pcOwner c) $ \owner -> putIntoPlay owner c
        "50095" -> fromGameValue (PerPlayer 1) >>= \n -> selectOne (EnvironmentWithTitle "Alert Level") >>= traverse_ (\eid -> removeCounters eid $ fromIntegral n)
        "50121" -> getDefeatedBy a >>= \you -> cardSpecific GameRef "aosReleaseHostage" (sideSchemeId a,you)
        "50173" -> do
          first <- selectJust FirstPlayer
          amount <- fromIntegral <$> fromGameValue (PerPlayer 1)
          removeSecrets first amount
        "50174" -> do
          first <- selectJust FirstPlayer
          amount <- fromIntegral <$> fromGameValue (PerPlayer 1)
          removeSecrets first amount
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "mgHoldCard" (fromJSON -> Success card) -> pure $ AosSideScheme a {sideSchemeHeldCards=sideSchemeHeldCards a <> [card]}
    CardSpecific (isTarget a -> True) "aosAbductAlly" (fromJSON -> Success you) -> do
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      chooseCharacterWithMostHP you (map AllyCharacter allies) $ \case
        AllyCharacter aid -> removeFromGame (AllyRef aid)
        _ -> pure ()
      when (null allies) $ placeThreatOn a (sideSchemeId a) 2
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      traverse_ (\(_,aid,_) -> tuckAlly aid) [(who,aid,def) | W.AllyLeavingPlay who aid def <- windows]
      pure e
    RanAbility _ (isTarget a -> True) 2 windows _ -> do
      for_ [(enemy,damage) | W.EnemyDamageWouldBeDealt enemy _ damage <- windows] $ \(enemy,damage) -> do
        cancelEnemyDamage enemy
        removeSideSchemeThreat (sideSchemeId a) (damageAmount damage)
      pure e
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      for_ [damageAmount d | W.EnemyWasDealtDamage _ d _ <- windows] $ \amount -> do
        placeThreatOn a (sideSchemeId a) amount
        cardSpecific a "aosCheckNuclearReaction" ()
      pure e
    CardSpecific (isTarget a -> True) "aosCheckNuclearReaction" _ -> do
      when (sideSchemeThreat a >= 10) do
        chars <- selectList AnyCharacter
        for_ chars $ \c -> damageCharacter c a (toDamage 10 FromAbility)
        removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) 5 windows _ -> do
      traverse_ cancelEncounterCard [card | W.EncounterCardRevealed _ _ card <- windows]
      pure e
    RanAbility _ (isTarget a -> True) 4 windows _ -> do
      for_ [(you,mid) | W.EngagedMinion you mid <- windows] $ \(you,mid) -> do
        def <- getsGame (`abilityCardDef` MinionRef mid)
        for_ def $ \card -> discardFromDeck you (boostCount $ cdBoostIcons card) a
      pure e
    WithDiscarded (isTarget a -> True) FromDeck cards | toCardCode a == "50162" -> do
      let n = count (== Energy) $ concatMap (printedResources . getCardDef) cards
      placeThreatOn a (sideSchemeId a) n
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "50122" -> e <$ dealBoost enemy
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "50140" -> do
      whenM (enemyMatches (EnemyWithTrait Trait.Aerial) enemy) $ dealBoost enemy
      pure e
    _ -> AosSideScheme <$> runMessage msg a
   where
    highAlert = selectAny (EnvironmentIs Cards.aos50090b)
    aimVictory = length . filter (cardMatch $ CardWithTrait Trait.Aim) <$> getVictoryDisplay
    tuckAlly aid = do
      card <- getsGame (`cardInPlay` AllyRef aid)
      for_ card $ \case
        PlayerCard c -> do
          cardSpecific GameRef "aosTuckAlly" (aid,sideSchemeId a,c)
          placeThreatOn a (sideSchemeId a) $ fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef c
          placeSideSchemeAccelerationToken (sideSchemeId a)
        _ -> pure ()


-- The explosion is one resolution; it cannot respond again to its own damage
-- before its final discard has resolved.
newtype AosNuclearReaction = AosNuclearReaction (Attrs SideScheme `With` Bool)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme AosNuclearReaction where
  toSideSchemeAttrs (AosNuclearReaction (a `With` _)) = a
  sideSchemeAbilities (AosNuclearReaction (a `With` exploding)) = if exploding then [] else getAbilities (AosSideScheme a)

instance HasModifiersFor AosNuclearReaction where
  getModifiersFor source target (AosNuclearReaction (a `With` _)) = getModifiersFor source target (AosSideScheme a)

instance RunMessage AosNuclearReaction where
  runMessage msg (AosNuclearReaction (a `With` exploding)) = runQueueT $ case msg of
    CardSpecific (isTarget a -> True) "aosCheckNuclearReaction" _ -> do
      when (not exploding && sideSchemeThreat a >= 10) do
        chars <- selectList AnyCharacter
        for_ chars $ \c -> damageCharacter c a (toDamage 10 FromAbility)
        removeFromPlay a
      pure $ AosNuclearReaction (a `With` (exploding || sideSchemeThreat a >= 10))
    _ -> do
      AosSideScheme updated <- runMessage msg (AosSideScheme a)
      pure $ AosNuclearReaction (updated `With` exploding)
