module Marvel.Shadowland.Traps where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Hp
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Question
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.GameValue
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Deck
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Trait qualified as T

newtype Trap = Trap (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsTreachery

traps :: [TreacheryCard Trap]
traps = map (treachery Trap) [Cards.street65050,Cards.street65051,Cards.street65052,Cards.street65053,Cards.street65054,Cards.street65055,Cards.street65056]

instance RunMessage Trap where
  runMessage msg e@(Trap a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      avoidAllowed <- not <$> selectAny (MinionIs Cards.street65047 <> MinionEngagedWith (IdentityWithId you))
      avoidPay <- canAffordCost you (toSource a) avoidCost
      disarmPay <- canAffordCost you (toSource a) disarmCost
      chooseOne you $
        [labeled "$label.slAvoid" (pick you "avoid" avoidCost) | avoidAllowed && avoidPay]
        <> [labeled "$label.slDisarm" (pick you "disarm" disarmCost) | disarmPay]
        <> [labeled "$label.slTrip" (checkWindows [W.Window W.When $ W.ChoseTrapOption you "trip"] >> cardSpecific a "trip" you)]
      pure e
    CardSpecific (isTarget a -> True) "avoid" _ -> pure e
    CardSpecific (isTarget a -> True) "disarm" (fromJSON -> Success (you, _ :: Payment)) -> do
      case toCardCode a of
        "65050" -> do
          chars <- selectList (CharacterControlledBy $ IdentityWithId you)
          chooseOne you [targeting (characterRef c) (giveStatus c ToughStatus) | c <- chars]
        "65051" -> do
          enemies <- selectList (EnemyCanTakeDamageFrom $ toSource a)
          chooseOne you [targeting (enemyRef enemy) (dealDamage enemy a $ toDamage 2 FromAbility) | enemy <- enemies]
        "65052" -> drawCards you 2
        "65053" -> selectOne EncounterMainScheme >>= traverse_ (\mid -> removeThreat you a 4 (SchemeWithId mid))
        "65054" -> selectOne ActiveVillain >>= traverse_ (\vid -> dealDamage vid a $ toDamage 3 FromAbility)
        "65055" -> search you (SearchIdentityDeck you $ TopOfDeck 4) AnyCard SearchDrawOne ShuffleBackIn
        "65056" -> removeThreat you a 3 ThwartableScheme
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "trip" (fromJSON -> Success you) -> do
      case toCardCode a of
        "65050" -> hurt you 2 >> confuseIdentity you
        "65051" -> hurt you 2 >> stunIdentity you
        "65052" -> hurt you 3 >> discardFromDeck you 3 a
        "65053" -> discardFromHand you 1 >> discardFromDeck you 8 a
        "65054" -> hurt you 3 >> exhaust you
        "65055" -> discardUntil FromEncounterDeck (CardWithType MinionType) a
        "65056" -> discardUntil FromEncounterDeck (CardWithType SideSchemeType) a
        _ -> pure ()
      pure e
    WithDiscardedMatch (isTarget a -> True) FromEncounterDeck (EncounterCard card) -> getYou >>= \you -> revealFoundEncounterCard you card >> pure e
    _ -> Trap <$> runMessage msg a
   where
    hurt you n = damageIdentity you a (toDamage n FromTreachery)
    pick you option cost = do
      checkWindows [W.Window W.When $ W.ChoseTrapOption you option]
      payCostThenWithPayment you a cost a option you
    avoidCost = case toCardCode a of
      "65050" -> discardHand $ CardWithType EventType <> CardWithTrait T.Thwart
      "65051" -> discardHand $ CardWithType EventType <> CardWithTrait T.Defense
      "65052" -> discardHand $ CardWithType AllyType
      "65053" -> discardHand $ CardWithType UpgradeType
      "65054" -> resources [Energy,Energy]
      "65055" -> resources [Mental,Mental]
      _ -> discardHand $ CardWithType EventType <> CardWithTrait T.Attack
    disarmCost = case toCardCode a of
      "65050" -> DiscardRandomHandCardsCost 2
      "65051" -> discardHand HeroSpecificCard
      "65052" -> DiscardControlledCardCost $ CardWithType AllyType
      "65053" -> DiscardControlledCardCost $ CardWithType UpgradeType
      "65054" -> resources [Mental,Mental,Physical]
      "65055" -> resources [Energy,Physical,Physical]
      _ -> DiscardControlledCardCost $ CardWithType SupportType
    discardHand = DiscardMatchingHandCardCost
    resources = MultiResourceCost . map Just


newtype IronMonk = IronMonk (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
ironMonk :: MinionCard IronMonk
ironMonk = minion IronMonk Cards.street65047 (Sch 2) (Atk 1) (HP 4)
instance RunMessage IronMonk where
  runMessage msg (IronMonk a) = IronMonk <$> runMessage msg a

newtype TrapScheme = TrapScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass HasModifiersFor
instance IsSideScheme TrapScheme where
  sideSchemeAbilities = getAbilities
trapSchemes :: [SideSchemeCard TrapScheme]
trapSchemes = [sideScheme TrapScheme Cards.street65048 (PerPlayer 2), sideScheme TrapScheme Cards.street65049 (Static 5)]
instance HasAbilities TrapScheme where
  getAbilities (TrapScheme a) = [windowAbility a 1 (W.TrapOptionChosen AnyIdentity options) ForcedInterrupt NoCost (runAbility a 1)]
   where options = if toCardCode a == "65048" then ["trip"] else ["avoid","trip"]
instance RunMessage TrapScheme where
  runMessage msg e@(TrapScheme a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [you | W.ChoseTrapOption you _ <- windows] $ \you ->
        if toCardCode a == "65048" then discardFromDeck you 3 a else dealIndirectDamage you a 2
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      chars <- selectList (CharacterControlledBy $ IdentityWithId you)
      if toCardCode a == "65048" then selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn a mid $ fromIntegral $ length chars)
        else traverse_ (\c -> damageCharacter c a $ toDamage 1 FromAbility) chars
      pure e
    _ -> TrapScheme <$> runMessage msg a

newtype NervousJitters = NervousJitters (Attrs Attachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsAttachment, HasModifiersFor)
nervousJitters :: AttachmentCard NervousJitters
nervousJitters = attachment NervousJitters Cards.street65046
instance HasAbilities NervousJitters where
  getAbilities (NervousJitters a) = [windowAbility a 1 (W.TrapOptionChosen (IdentityWithId you) ["avoid","disarm"]) ForcedInterrupt NoCost (runAbility a 1) | Just you <- [attachmentIdentity a]]
instance RunMessage NervousJitters where
  runMessage msg e@(NervousJitters a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> attachToIdentity aid you >> pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      chooseOne you [labeled label $ discardTopOfEncounterDeckThen 1 $ \cards ->
        unless (any (cardMatch $ CardWithType kind) cards) $ dealIndirectDamage you a 4 >> removeFromPlay a
        | (label,kind) <- [("$label.slNameMinion",MinionType),("$label.slNameTreachery",TreacheryType),("$label.slNameAttachment",AttachmentType),("$label.slNameSideScheme",SideSchemeType),("$label.slNameObligation",ObligationType),("$label.slNameEnvironment",EnvironmentType),("$label.slNameVillain",VillainType),("$label.slNameMainScheme",MainSchemeType)]]
      pure e
    _ -> NervousJitters <$> runMessage msg a
