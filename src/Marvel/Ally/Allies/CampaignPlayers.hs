{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.CampaignPlayers where
import Marvel.Prelude
import Marvel.Name qualified as Name
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Phase
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types qualified as R
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.CampaignPlayers.Cards qualified as Cards
import Marvel.CampaignPlayers.Helpers
import Marvel.FalconWinter.Helpers (nextCardReduction)
import Marvel.Ally.Types
import Marvel.Ally.Runner ()

cw56002 :: AllyCard CampaignAlly
cw56002 = ally CampaignAlly Cards.cw56002 (Thw 2, 1) (Atk 2, 1) (HP 3)

cw56010 :: AllyCard CampaignAlly
cw56010 = ally CampaignAlly Cards.cw56010 (Thw 1, 1) (Atk 2, 1) (HP 3)

cw56019 :: AllyCard CampaignAlly
cw56019 = ally CampaignAlly Cards.cw56019 (Thw 2, 2) (Atk 2, 2) (HP 3)

cw56030 :: AllyCard CampaignAlly
cw56030 = ally CampaignAlly Cards.cw56030 (Thw 2, 1) (Atk 1, 1) (HP 2)

cw56041 :: AllyCard CampaignAlly
cw56041 = ally CampaignAlly Cards.cw56041 (Thw 1, 1) (Atk 2, 1) (HP 3)

cw56052 :: AllyCard CampaignAlly
cw56052 = ally CampaignAlly Cards.cw56052 (Thw 1, 1) (Atk 1, 1) (HP 3)

fne60007 :: AllyCard CampaignAlly
fne60007 = ally CampaignAlly Cards.fne60007 (Thw 1, 1) (Atk 2, 1) (HP 3)

fne60019 :: AllyCard CampaignAlly
fne60019 = ally CampaignAlly Cards.fne60019 (Thw 2, 1) (Atk 2, 1) (HP 3)

fne60020 :: AllyCard CampaignAlly
fne60020 = ally CampaignAlly Cards.fne60020 (Thw 2, 0) (Atk 1, 0) (HP 2)

fne60021 :: AllyCard CampaignAlly
fne60021 = ally CampaignAlly Cards.fne60021 (Thw 1, 2) (Atk 2, 2) (HP 3)

fne60022 :: AllyCard CampaignAlly
fne60022 = ally CampaignAlly Cards.fne60022 (Thw 1, 1) (Atk 2, 1) (HP 3)

fne60039 :: AllyCard CampaignAlly
fne60039 = ally CampaignAlly Cards.fne60039 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype CampaignAlly = CampaignAlly (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CampaignAlly where
  getAbilities a@(CampaignAlly attrs) = case Name.title (cdName (allyCardDef attrs)) of
    "Moon Knight" -> [limitedWindowAbility a 1 W.EntersPlayThis ForcedResponse OwnsThis NoCost $ runAbility a 1]
    "Two-Gun Kid" -> [limitedWindowAbility a 1 (W.AllyAttacked W.When self AnyEnemy) Interrupt (OwnsThis <> AllyHasTwoAttackableEnemies (allyId attrs)) NoCost $ runAbility a 1]
    "Yellow Jacket" -> [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost $ runAbility a 1]
    "Wiccan" -> [limitedWindowAbility a 1 (W.PlayedFromHand you $ CardIs Cards.cw56030) Response OwnsThis NoCost $ runAbility a 1]
    "Patriot" -> [limitedWindowAbility a 1 (W.EngagesMinion W.After you AnyMinion) Response OwnsThis NoCost $ runAbility a 1]
    "Iron Lad" -> [limitedWindowAbility a 1 (W.WindowOneOf [W.AllyAttacked W.When self AnyEnemy, W.AllyThwarted W.When self AnyScheme]) Interrupt OwnsThis (ExhaustAlliesMatchingCost (AllyWithTrait T.Avenger <> AllyControlledBy you <> NotAlly self) 1) $ runAbility a 1]
    "Elektra" -> [limitedWindowAbility a 1 (W.AllyWouldTakeDamageOf W.When self FromConsequential) ForcedInterrupt (OwnsThis <> InHeroForm <> CharacterExists (IdentityCharacters $ IdentityIs Cards.fne60001a)) NoCost $ runAbility a 1]
    "Blindspot" -> [limitedWindowAbility a 1 (W.AllyThwarted W.After self AnyScheme) Response (OwnsThis <> EnemyExists (EnemyWithUpgrade (UpgradeMatches []) <> EnemyCanBeConfused)) NoCost $ runAbility a 1]
    "Cloak" -> [ability a 1 Action (OwnsThis <> CanFindPlayerCardForEntry "60021") (ExhaustCost <> MultiResourceCost [Just R.Energy,Just R.Energy]) $ runAbility a 1]
    "Ghost Rider" -> [limitedWindowAbility a 1 (W.AllyAttacked W.When self enemy) Interrupt OwnsThis cost $ runAbility a 1
      | (enemy,cost) <- [(MinionEnemy <> EnemyCanBeConfused,ResourceCost $ Just R.Energy),(VillainEnemy <> EnemyCanBeConfused,MultiResourceCost [Just R.Energy,Just R.Energy])]]
    "Daredevil" -> [limitedWindowAbility a 1 (W.WindowOneOf [W.AllyAttacked W.After self AnyEnemy,W.AllyThwarted W.After self AnyScheme]) Response OwnsThis NoCost $ runAbility a 1]
    _ -> []
   where
    self = AllyWithId $ allyId attrs
    you = IdentityWithId $ allyController attrs

instance HasModifiersFor CampaignAlly where
  getModifiersFor _ target (CampaignAlly attrs) = case Name.title (cdName (allyCardDef attrs)) of
    "Dagger" -> do
      cloak <- selectAny $ AllyWithTitle "Cloak"
      cloaks <- selectList $ AllyWithTitle "Cloak"
      pure $ [ConsequentialDamageReduction 1 | target == toRef attrs && cloak]
        <> [SuppressSchemeIcons | target `elem` map AllyRef cloaks]
    _ -> pure []

instance RunMessage CampaignAlly where
  runMessage msg a@(CampaignAlly attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      case Name.title (cdName (allyCardDef attrs)) of
        "Moon Knight" -> do
          deck <- getEncounterDeck
          discard <- getEncounterDiscard
          let minions = filter (cardMatch $ CardWithType MinionType) (deck <> discard)
          when (null minions) shuffleEncounterDeck
          unless (null minions) $ chooseOneSearchedIn you "$label.searchEncounterDeck" [("$label.encounterDeck",map EncounterCard deck),("$label.encounterDiscard",map EncounterCard discard)]
            [targeting card do
              putEncounterCardIntoPlay you card
              let mid = MinionId $ unCardId $ ecCardId card
              giveStatus mid StunnedStatus
              giveStatus mid ConfusedStatus
              shuffleEncounterDeck
            | card <- minions]
        "Two-Gun Kid" -> do
          enemies <- selectList $ AttackableByAlly $ allyId attrs
          chooseOne you [targeting (enemyRef first) (chooseOne you [targeting (enemyRef second) (resolveAllyAttackAgainstAll (allyId attrs) [first,second]) | second <- enemies, second /= first]) | first <- enemies, length enemies >= 2]
        "Yellow Jacket" -> search you (SearchIdentityDeck you $ TopOfDeck 5) (CardWithType UpgradeType) SearchDrawOne ShuffleBackIn
        "Wiccan" -> searchOwned you attrs (HeroSpecificCard <> CardWithType EventType) True
        "Patriot" -> giveStatus (allyId attrs) ToughStatus
        "Iron Lad" -> for_ (paymentExhaustedAllies payment) $ \aid -> do
          let thwarting = any (\case W.AllyThwart _ _ -> True; _ -> False) windows
          amount <- if thwarting then getCharacterThwart $ AllyCharacter aid else characterAttackValue $ AllyCharacter aid
          addToAllyBasicPower (allyId attrs) amount
        "Elektra" -> do
          pending <- pendingCharacterDamage $ allyId attrs
          for_ pending $ \(source,damage) -> do
            cancelMatchingMessage $ \case AllyMessage aid (AllyDamaged _ d) -> aid == allyId attrs && damageSource d == FromConsequential; _ -> False
            daredevils <- selectList $ IdentityIs Cards.fne60001a
            traverse_ (\ident -> damageIdentity ident source damage) daredevils
        "Blindspot" -> chooseEnemyStatus you ConfusedStatus $ EnemyWithUpgrade (UpgradeMatches [])
        "Cloak" -> do
          found <- findPlayerCardsForEntry you "60021"
          unless (null found) $ chooseOrRunOne you [targeting card (putIntoPlay you card) | card <- found]
        "Ghost Rider" -> for_ [enemy | W.AllyAttack _ enemy <- windows] $ \enemy -> giveStatus enemy ConfusedStatus
        "Daredevil" -> nextEventReductionForRound you attrs 1
        _ -> pure ()
      pure a
    _ -> CampaignAlly <$> runMessage msg attrs
