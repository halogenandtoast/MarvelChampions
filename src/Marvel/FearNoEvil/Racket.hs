module Marvel.FearNoEvil.Racket where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Types qualified as A
import Marvel.Ally.Runner ()
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Effect.Types qualified as E
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype Business = Business (Attrs MainScheme `With` (Bool,Bool))
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
instance IsMainScheme Business where
  toMainSchemeAttrs (Business (a `With` _)) = a
  mainSchemeAbilities = getAbilities
  mainSchemeModifiersFor _ (IdentityRef _) (Business (a `With` (used,_))) = pure
    [UpgradeInPlayAreaCostReduction you 1 | toCardCode a == "60137b",not used,Just you <- [mainSchemeOwner a]]
  mainSchemeModifiersFor _ _ _ = pure []
businesses :: [MainSchemeCard Business]
businesses = [mainScheme (\a -> Business $ a `With` (False,False)) def (Static 10) (Static 0) (Static 1) | def <- [Cards.street60134b,Cards.street60135b,Cards.street60136b,Cards.street60137b,Cards.street60138b]]
instance HasAbilities Business where
  getAbilities (Business (a `With` (used,expert))) = case mainSchemeOwner a of
    Nothing -> []
    Just you -> case toCardCode a of
      "60134b" -> [windowAbility a 1 (W.CharacterAttackedInPlayArea you) ForcedResponse NoCost (runAbility a 1) | expert || not used]
      "60135b" -> [windowAbility a 2 (W.ExcessDamageInPlayArea you expert) ForcedInterrupt NoCost (runAbility a 2)]
      "60136b" -> [windowAbility a 3 (W.CharacterEnteredInPlayArea you) ForcedResponse NoCost (runAbility a 3)]
      "60137b" -> [windowAbility a 4 (W.CardEnteredInPlayArea you $ CardMatchesAny [CardWithType UpgradeType,CardWithType AttachmentType]) ForcedResponse NoCost (runAbility a 4)]
      _ -> [windowAbility a 5 (W.CharacterDefeatsInPlayArea you) ForcedResponse NoCost (runAbility a 5)]
instance RunMessage Business where
  runMessage msg e@(Business (a `With` state@(used,expert))) = runQueueT $ case msg of
    SetupMainScheme | isNothing (mainSchemeOwner a) -> getAllPlayers >>= cardSpecific GameRef "racketChooseBusinesses" >> pure e
    CardSpecific (isTarget a -> True) "racketDifficulty" (fromJSON -> Success hard) -> pure $ Business (a `With` (used,hard))
    BeginPhase _ | toCardCode a == "60134b" -> pure $ Business (a `With` (False,expert))
    EndRound | toCardCode a == "60137b" -> pure $ Business (a `With` (False,expert))
    CardSpecific GameRef "upgradePlayedInArea" (fromJSON -> Success (controller :: Maybe IdentityId)) | toCardCode a == "60137b" -> do
      pure $ Business (a `With` (used || controller == mainSchemeOwner a,expert))
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      records <- concatMapM W.attackedCharacters windows
      relevant <- filterM (fmap (== mainSchemeOwner a) . characterPlayAreaOwner . snd) records
      for_ (if expert then relevant else take 1 relevant) $ \(attacker,_) -> do
        character <- threatSourceCharacter attacker
        for_ character $ \target -> damageCharacter target a $ toDamage 1 FromAbility
        placeThreatOn a (mainSchemeId a) 1
      pure $ Business (a `With` (True,expert))
    RanAbility _ (isTarget a -> True) 2 windows _ -> do
      for_ [n | W.CharacterTookExcessDamage _ _ n <- windows] $ placeThreatOn a (mainSchemeId a)
      pure e
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      for_ (mapMaybe W.enteredCharacter windows) $ \character -> do
        damageCharacter character a $ toDamage 1 FromAbility
        placeThreatOn a (mainSchemeId a) 1
      pure e
    RanAbility _ (isTarget a -> True) 4 _ _ -> placeThreatOn a (mainSchemeId a) 1 >> pure e
    RanAbility _ (isTarget a -> True) 5 windows _ -> do
      for_ windows $ \window -> do
        character <- W.defeatingCharacter window
        for_ character $ \target -> healFrom a target 1 >> placeThreatOn a (mainSchemeId a) 1
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a -> gameOver Lost >> pure e
    _ -> (\attrs -> Business $ attrs `With` state) <$> runMessage msg a

newtype ShopProprietor = ShopProprietor (Attrs A.Ally)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (A.IsAlly,HasModifiersFor)
shopProprietor :: A.AllyCard ShopProprietor
shopProprietor = A.ally ShopProprietor Cards.street60139 (Thw 1,2) (Atk 1,2) (HP 3)
instance HasAbilities ShopProprietor where
  getAbilities (ShopProprietor a) = [windowAbility a 1 (W.AllyLeavesPlay W.When (IdentityWithId $ A.allyController a) $ CardIs $ getCardDef a) ForcedInterrupt NoCost (runAbility a 1)]
instance RunMessage ShopProprietor where
  runMessage msg e@(ShopProprietor a) = runQueueT $ case msg of
    AllyMessage aid (AllyRevealed you) | aid == A.allyId a -> surge you >> pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      matcher <- mainSchemeMatcherForSource (A.allyController a) (toSource a) EncounterMainScheme
      schemes <- selectList matcher
      for_ schemes $ \scheme -> placeThreatOn a scheme 4
      pure e
    _ -> ShopProprietor <$> runMessage msg a

newtype ManBull = ManBull (Attrs Minion `With` Natural)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass HasModifiersFor
instance IsMinion ManBull where
  toMinionAttrs (ManBull (a `With` _)) = a
  overMinionAttrs f (ManBull (a `With` n)) = ManBull (f a `With` n)
manBull :: MinionCard ManBull
manBull = minion (\a -> ManBull $ a `With` 0) Cards.street60140 (Sch 2) (Atk 4) (HP 6)
instance HasAbilities ManBull where
  getAbilities (ManBull (a `With` n)) = [windowAbility a 1 (W.EnemyAttackedAnyTarget W.After $ EnemyWithId $ toEnemyId a) ForcedResponse NoCost (runAbility a 1) | n > 0]
instance RunMessage ManBull where
  runMessage msg e@(ManBull (a `With` n)) = runQueueT $ case msg of
    CardSpecific (isTarget a -> True) "attackExcessDamage" (fromJSON -> Success extra) -> pure $ ManBull (a `With` (n+extra))
    RanAbility _ (isTarget a -> True) 1 _ _ -> placeThreatOnEach a n EncounterMainScheme >> pure e
    MinionMessage mid MinionEndAttack | mid == minionId a -> do
      attrs <- runMessage msg a
      pure $ ManBull (attrs `With` 0)
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      attacking <- isJust <$> getEnemyAttack enemy
      when attacking $ createEffect Cards.street60140 a (EnemyEntity $ EnemyWithId enemy)
      pure e
    _ -> (\attrs -> ManBull $ attrs `With` n) <$> runMessage msg a

newtype ManBullBoost = ManBullBoost (Attrs E.Effect `With` Natural)
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
instance E.IsEffect ManBullBoost where
  toEffectAttrs (ManBullBoost (a `With` _)) = a
  effectAbilities = getAbilities
manBullBoost :: E.CardEffect ManBullBoost
manBullBoost = E.effect (\a -> ManBullBoost $ a `With` 0) Cards.street60140
instance HasAbilities ManBullBoost where
  getAbilities (ManBullBoost (a `With` n)) = [windowAbility a 1 (W.EnemyAttackedAnyTarget W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 1) | EnemyEntity (EnemyWithId enemy) <- [E.effectMatcher a],n > 0]
instance RunMessage ManBullBoost where
  runMessage msg e@(ManBullBoost (a `With` n)) = runQueueT $ case E.effectMatcher a of
    EnemyEntity (EnemyWithId enemy) -> case msg of
      CardSpecific target "attackExcessDamage" (fromJSON -> Success extra) | target == enemyRef enemy -> pure $ ManBullBoost (a `With` (n+extra))
      RanAbility _ (isTarget a -> True) 1 _ _ -> placeThreatOnEach (enemyRef enemy) n EncounterMainScheme >> pure e
      VillainMessage vid VillainEndAttack | enemy == EnemyVillainId vid -> disableEffect (E.effectId a) >> pure e
      MinionMessage mid MinionEndAttack | enemy == EnemyMinionId mid -> disableEffect (E.effectId a) >> pure e
      _ -> (\attrs -> ManBullBoost $ attrs `With` n) <$> runMessage msg a
    _ -> pure e

newtype ChangeOfVenue = ChangeOfVenue (Attrs Treachery)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsTreachery,HasAbilities,HasModifiersFor)
changeOfVenue :: TreacheryCard ChangeOfVenue
changeOfVenue = treachery ChangeOfVenue Cards.street60141
instance RunMessage ChangeOfVenue where
  runMessage msg e@(ChangeOfVenue a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> cardSpecific GameRef "racketSwapBusiness" you >> pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      -- FFG's September 22, 2026 ruling: resolve only When Revealed, without surge.
      getBoostedPlayer enemy >>= cardSpecific GameRef "racketSwapBusiness"
      pure e
    _ -> ChangeOfVenue <$> runMessage msg a
