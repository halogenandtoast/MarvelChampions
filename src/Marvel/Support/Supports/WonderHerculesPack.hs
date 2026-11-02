module Marvel.Support.Supports.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Effect.Types
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Hercules.Helpers
import Marvel.Identity.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.WonderMan.Cameo
import Marvel.WonderMan.Helpers
import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Types
import Marvel.WonderMan.Cards qualified as WM
import Marvel.Hercules.Cards qualified as HC

wm58007 :: SupportCard WmHcSupport
wm58007 = support WmHcSupport WM.wm58007

wm58024 :: SupportCard WmHcSupport
wm58024 = support WmHcSupport WM.wm58024

wm58031 :: SupportCard WmHcSupport
wm58031 = support WmHcSupport WM.wm58031

wm58034 :: SupportCard WmHcSupport
wm58034 = support WmHcSupport WM.wm58034

hc59012 :: SupportCard WmHcSupport
hc59012 = support WmHcSupport HC.hc59012

hc59023 :: SupportCard WmHcSupport
hc59023 = support WmHcSupport HC.hc59023

newtype WmHcSupport = WmHcSupport (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WmHcSupport where
  getAbilities s@(WmHcSupport attrs) = case canonicalCardCode (supportCardDef attrs) of
    "58007" -> [ability s 1 AlterEgoAction OwnsThis ExhaustCost $ runAbility s 1]
    "58024" -> [limitedWindowAbility s 1 W.AvengerHeroChangedToAlterEgo Response OwnsThis ExhaustCost $ runAbility s 1]
    "58031" -> [ability s 1 Setup OwnsThis NoCost $ runAbility s 1]
    "58034" -> [ability s 1 Action (OwnsThis <> if null (supportHeldCards attrs) then ExtendedCardExists (InHandOf You $ BasicCardMatches $ CardWithType AllyType) else HeldAllyPlayable (supportId attrs)) ExhaustCost $ runAbility s 1]
    "59012" -> [ability s 1 Resource (OwnsThis <> HasEffectiveHerculesGift) ExhaustCost $ runAbility s 1]
    "59023" -> [ability s 1 Action (OwnsThis <> InAlterEgoForm) (DiscardCost $ toRef attrs) $ runAbility s 1]
    _ -> []

instance RunMessage WmHcSupport where
  runMessage msg s@(WmHcSupport attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      case canonicalCardCode (supportCardDef attrs) of
        "58007" -> do
          cards <- filter (\card -> hasPrintedEnergy card && cardMatch (CardWithType EventType) card) <$> projectMap PlayerIdentityDiscard unDiscard you
          tucked <- ionicCards you
          chooseOne you $ [labeled "$label.draw1Card" $ drawCards you 1] <> [targeting card (tuckIonicCard you card) | length tucked < 3, card <- cards]
        "58024" -> for_ [who | W.AvengerChangedToAlterEgo who <- windows] $ \who -> do
          statuses <- filterM (fmap (>0) . getCharacterStatusCount (IdentityCharacter who)) [StunnedStatus, ConfusedStatus, ToughStatus]
          chooseOne you $ [labeled "$label.recover" $ lastingModifiers attrs (IdentityEntity $ IdentityWithId who) [RecoveryModifier 2] False]
            <> [labeled ("$label.discard" <> tshow status) $ removeStatus who status | status <- statuses]
        "58031" -> do
          choices <- cameoChoices
          chooseOne you [cardLabeled (toCardCode def) do
            cid <- getRandom
            shuffleIntoDeck you [MkPlayerCard cid def (Just you) (Just you)]
            discardFromHand you 2
            removeFromGame attrs
            | def <- choices]
        "58034" -> do
          let held = supportHeldCards attrs
          if null held then do
            allies <- filter (cardMatch $ CardWithType AllyType) <$> projectMap PlayerIdentityHand unHand you
            chooseOne you [targeting card $ tuckPlayerCardUnder attrs card | card <- allies]
          else chooseOne you [targeting card $ playHeldCardAsHand you attrs card | card <- held]
        "59012" -> do
          n <- giftCount you
          paid $ mconcat $ replicate (fromIntegral n) $ ResourcePayment Wild
        "59023" -> do
          players <- selectList AnyIdentity
          chooseOne you [targeting who $ createEffect HC.hc59023 attrs (IdentityEntity $ IdentityWithId who) | who <- players]
        _ -> pure ()
      pure s
    CheckWindows windows | canonicalCardCode (supportCardDef attrs) == "59023", any (\case W.Window W.After (W.IdentityChangesForm who) -> who == supportController attrs; _ -> False) windows -> do
      hero <- identityMatches HeroIdentity $ supportController attrs
      when hero $ removeFromPlay attrs
      pure s
    _ -> WmHcSupport <$> runMessage msg attrs

newtype RecruitmentDriveEffect = RecruitmentDriveEffect (Attrs Effect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

recruitmentDriveEffect :: CardEffect RecruitmentDriveEffect
recruitmentDriveEffect = effectWith RecruitmentDriveEffect HC.hc59023 (endsL ?~ DisableAtEndOfPhase)

instance RunMessage RecruitmentDriveEffect where
  runMessage msg e@(RecruitmentDriveEffect attrs) = runQueueT $ case msg of
    IdentityMessage you (PlayedCard card _) | cardMatch (CardWithType AllyType) card -> do
      whenM (effectValidFor attrs $ IdentityRef you) $ disableEffect $ effectId attrs
      pure e
    _ -> RecruitmentDriveEffect <$> runMessage msg attrs

instance IsEffect RecruitmentDriveEffect where
  getEffectModifiers (RecruitmentDriveEffect attrs) = case effectMatcher attrs of
    IdentityEntity (IdentityWithId you) -> do
      cards <- filter (cardMatch $ CardWithType AllyType) <$> getPlayableHandCards you
      pure [IgnoresResourceCostForCard $ pcCardId card | card <- cards]
    _ -> pure []
