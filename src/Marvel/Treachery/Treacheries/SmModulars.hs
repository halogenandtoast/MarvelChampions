module Marvel.Treachery.Treacheries.SmModulars where

import Marvel.Prelude
import Control.Monad (replicateM_)
import Data.HashMap.Strict qualified as HashMap
import Marvel.AlterEgo.Types qualified as AlterEgo
import Marvel.Keyword (Keyword (Overkill))
import Marvel.Minion.Types qualified as Minion
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Effect.Types
import Marvel.Id
import Marvel.Query
import Marvel.Window qualified as W
import Marvel.Ability.Types
import Marvel.Card
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (PlayerIdentitySide (..), Field (PlayerIdentityHand, PlayerIdentitySide, PlayerIdentitySides))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

data SmModularTreacheryMeta = SmModularTreacheryMeta
  { looseEndsPlayer :: Maybe IdentityId
  , looseEndsObligation :: Maybe ObligationId
  , looseEndsRevealed :: Bool
  , looseEndsChanged :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype SmModularsTreachery = SmModularsTreachery (Attrs Treachery `With` SmModularTreacheryMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery SmModularsTreachery where
  toTreacheryAttrs (SmModularsTreachery (attrs `With` _)) = attrs

smModularsTreachery :: Attrs Treachery -> SmModularsTreachery
smModularsTreachery attrs = SmModularsTreachery (attrs `With` SmModularTreacheryMeta Nothing Nothing False False)

smCallingInFavors :: TreacheryCard SmModularsTreachery
smCallingInFavors = treachery smModularsTreachery Cards.smCallingInFavors

smNowOrNever :: TreacheryCard SmModularsTreachery
smNowOrNever = treachery smModularsTreachery Cards.smNowOrNever

smThreatOrMenace :: TreacheryCard SmModularsTreachery
smThreatOrMenace = treachery smModularsTreachery Cards.smThreatOrMenace

smLooseEnds :: TreacheryCard SmModularsTreachery
smLooseEnds = treachery smModularsTreachery Cards.smLooseEnds

smRemoteNavigation :: TreacheryCard SmModularsTreachery
smRemoteNavigation = treachery smModularsTreachery Cards.smRemoteNavigation

smFromEveryDirection :: TreacheryCard SmModularsTreachery
smFromEveryDirection = treachery smModularsTreachery Cards.smFromEveryDirection

smDeepestFears :: TreacheryCard SmModularsTreachery
smDeepestFears = treachery smModularsTreachery Cards.smDeepestFears

smSwingingAssault :: TreacheryCard SmModularsTreachery
smSwingingAssault = treachery smModularsTreachery Cards.smSwingingAssault

smUnstableSentience :: TreacheryCard SmModularsTreachery
smUnstableSentience = treachery smModularsTreachery Cards.smUnstableSentience

instance RunMessage SmModularsTreachery where
  runMessage msg t@(SmModularsTreachery (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs, toCardCode attrs == "27141" -> do
      glider <- selectAny (AttachmentIs Attachments.smAdvancedGlider)
      if glider then withTheVillain you (`villainActivatesAgainst` you)
        else do
          deck <- getEncounterDeck
          discard <- getEncounterDiscard
          let found = filter (cardMatch $ CardIs Attachments.smAdvancedGlider) (deck <> discard)
          unless (null found) $ chooseOrRunOne you
            [targeting card (revealEncounterCardFrom (if card `elem` discard then W.RevealedFromDiscardPile else W.RevealedFromEncounterDeck) you card) | card <- found]
          shuffleEncounterDeck
      pure t
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs, toCardCode attrs == "27168" -> do
      hero <- identityMatches HeroIdentity you
      unless hero $ changeToHeroForm you
      currentVillainAttacksWithAdditionalBoostCards you (if hero then 1 else 0)
      pure t
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs, toCardCode attrs == "27169" -> do
      withTheVillain you dealBoost
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27141" -> do
      replicateM_ 2 (dealBoost enemy)
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27169" -> do
      attacking <- enemyMatches AttackingEnemy enemy
      when attacking $ addEnemyBoost enemy 2 >> attackGains enemy Overkill
      pure t
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs, toCardCode attrs == "27129" -> do
      rhinos <- selectList (MinionIs Minions.smRhino)
      case rhinos of
        mid : _ -> do
          createEffect Cards.smCallingInFavors (MinionRef mid) (MinionEntity $ MinionWithId mid)
          minionSchemes mid
          cardSpecific attrs "callingInFavorsFinished" mid
        [] -> do
          deck <- getEncounterDeck
          discard <- getEncounterDiscard
          let found = filter (cardMatch $ CardWithTitle "Rhino" <> CardWithType MinionType) (deck <> discard)
          unless (null found) $ chooseOrRunOne you
            [targeting card (putEncounterCardIntoPlay you card) | card <- found]
          shuffleEncounterDeck
      pure t
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs, toCardCode attrs == "27130" -> do
      let cost = ExhaustCharacterCost (CharacterControlledBy $ IdentityWithId you) <> ResourceCost Nothing
      affordable <- canAffordCost you (toSource attrs) cost
      mainSchemes <- selectList EncounterMainScheme
      chooseOrRunOne you $
        [labeled "$label.smNowOrNeverAcceleration" (for_ mainSchemes \sid -> case sid of SchemeMainSchemeId mid -> placeAccelerationToken mid; _ -> pure ())]
        <> [labeled "$label.smNowOrNeverExhaust" do
            continuation <- capture (pure ())
            payCostThen you attrs cost continuation
           | affordable]
      pure t
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs, toCardCode attrs == "27134" -> do
      canChange <- identityMatches IdentityCanChangeForm you
      chooseOne you $
        [ labeled "$label.changeForm" (flipIdentity you >> cardSpecific attrs "threatOrMenace" you) | canChange ]
        <> [ labeled "$label.doNotChangeForm" (cardSpecific attrs "threatOrMenace" you)
        ]
      pure t
    CardSpecific (isTarget attrs -> True) "threatOrMenace" (fromJSON -> Success you) -> do
      hero <- identityMatches HeroIdentity you
      if hero then placeThreatOnEach attrs 2 EncounterMainScheme
        else createEffect Cards.smThreatOrMenace attrs (IdentityEntity $ IdentityWithId you)
      pure t
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs, toCardCode attrs == "27135" -> do
      sides <- project PlayerIdentitySides you
      let obligations = concat [AlterEgo.alterEgoObligations (toAttrs ae) | AlterEgoSide ae <- HashMap.elems sides]
      deck <- getEncounterDeck
      discard <- getEncounterDiscard
      aside <- getSetAsideEncounterCards
      removed <- getRemovedFromGameEncounterCards
      let candidates = [(source, card) | (source, cards) <- [(W.RevealedFromEncounterDeck, deck), (W.RevealedFromDiscardPile, discard), (W.RevealedFromSetAside, aside), (W.RevealedFromRemovedFromGame, removed)], card <- cards, getCardDef card `elem` obligations]
      if null candidates then pure $ SmModularsTreachery ((attrs & surgeL .~ True) `With` meta)
        else do
          chooseOrRunOne you [targeting card (cardSpecific attrs "looseEndsReveal" (you, source, card)) | (source, card) <- candidates]
          pure t
    CardSpecific (isTarget attrs -> True) "looseEndsReveal" (fromJSON -> Success (you, source, card :: EncounterCard)) -> do
      revealEncounterCardFrom source you card
      cardSpecific attrs "looseEndsFinished" ()
      shuffleEncounterDeck
      pure $ SmModularsTreachery (attrs `With` SmModularTreacheryMeta (Just you) (Just $ ObligationId $ unCardId $ ecCardId card) False False)
    ObligationMessage oid (RevealObligation _) | Just oid == looseEndsObligation meta ->
      pure $ SmModularsTreachery (attrs `With` meta {looseEndsRevealed = True})
    CheckWindows windows | Just you <- looseEndsPlayer meta, any (changedToAE you) windows -> do
      ae <- identityMatches AlterEgoIdentity you
      pure $ SmModularsTreachery (attrs `With` meta {looseEndsChanged = looseEndsChanged meta || ae})
    CardSpecific (isTarget attrs -> True) "looseEndsFinished" _ -> do
      when (looseEndsRevealed meta && looseEndsChanged meta) $ for_ (looseEndsPlayer meta) (\you -> discardFrom you RandomFromHand 1)
      unless (looseEndsRevealed meta) $ for_ (looseEndsPlayer meta) surge
      pure $ SmModularsTreachery (attrs `With` SmModularTreacheryMeta Nothing Nothing False False)
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId attrs, toCardCode attrs == "27146" -> do
      n <- fromIntegral . length <$> selectList AnyEnemy
      when (n > 0) $ placeThreatOnEach attrs n EncounterMainScheme
      difficulty <- getDifficulty
      pure $ SmModularsTreachery ((attrs & surgeL .~ (difficulty == Expert)) `With` meta)
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs, toCardCode attrs == "27157" -> do
      n <- projectMap PlayerIdentityHand (fromIntegral . length . unHand) you
      if n == 0
        then damageIdentity you attrs (toDamage 1 FromTreachery)
        else discardFromDeck you n attrs
      pure $ SmModularsTreachery ((attrs & resolverL ?~ you) `With` meta)
    WithDiscarded (isTarget attrs -> True) FromDeck cards | toCardCode attrs == "27157" -> do
      you <- maybe getYou pure (treacheryResolver attrs)
      if any (cardMatch HeroSpecificCard) cards
        then placeThreatOnEach attrs 1 EncounterMainScheme
        else damageIdentity you attrs (toDamage 1 FromTreachery)
      pure t
    _ -> (\a -> SmModularsTreachery (a `With` meta)) <$> runMessage msg attrs

changedToAE :: IdentityId -> W.Window -> Bool
changedToAE you (W.Window W.After (W.IdentityChangesForm who)) = you == who
changedToAE _ _ = False

newtype CallingInFavorsEffect = CallingInFavorsEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
callingInFavorsEffect :: CardEffect CallingInFavorsEffect
callingInFavorsEffect = effectWith CallingInFavorsEffect Cards.smCallingInFavors (modifiersL .~ [SchemeModifier 2])
instance RunMessage CallingInFavorsEffect where
  runMessage msg e@(CallingInFavorsEffect attrs) = runQueueT $ case msg of
    CheckWindows windows | any (schemedBySource attrs) windows -> e <$ disableEffect (effectId attrs)
    CardSpecific _ "callingInFavorsFinished" (fromJSON -> Success mid) | effectSource attrs == MinionRef mid -> e <$ disableEffect (effectId attrs)
    _ -> CallingInFavorsEffect <$> runMessage msg attrs
schemedBySource :: Attrs Effect -> W.Window -> Bool
schemedBySource attrs (W.Window W.After (W.SchemedByMinion mid)) = effectSource attrs == MinionRef mid
schemedBySource _ _ = False

newtype ThreatOrMenaceEffect = ThreatOrMenaceEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
threatOrMenaceEffect :: CardEffect ThreatOrMenaceEffect
threatOrMenaceEffect = effect ThreatOrMenaceEffect Cards.smThreatOrMenace
instance RunMessage ThreatOrMenaceEffect where
  runMessage msg e@(ThreatOrMenaceEffect attrs) = runQueueT $ case msg of
    IdentityMessage you BeginTurn -> do
      relevant <- effectValidFor attrs (IdentityRef you)
      pure $ ThreatOrMenaceEffect (if relevant then attrs & modifiersL .~ [CannotChangeForm] else attrs)
    IdentityEndedTurn you | CannotChangeForm `elem` effectModifiers attrs -> do
      whenM (effectValidFor attrs $ IdentityRef you) $ disableEffect (effectId attrs)
      pure e
    _ -> ThreatOrMenaceEffect <$> runMessage msg attrs
