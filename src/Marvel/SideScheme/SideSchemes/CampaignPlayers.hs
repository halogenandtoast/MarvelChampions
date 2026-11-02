{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.SideScheme.SideSchemes.CampaignPlayers where
import Marvel.Prelude
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
import Marvel.SideScheme.Types

fne60012 :: SideSchemeCard CampaignSideScheme
fne60012 = sideScheme CampaignSideScheme Cards.fne60012 (Static 4)

fne60024 :: SideSchemeCard CampaignSideScheme
fne60024 = sideScheme CampaignSideScheme Cards.fne60024 (Static 4)

newtype CampaignSideScheme = CampaignSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CampaignSideScheme

instance RunMessage CampaignSideScheme where
  runMessage msg s@(CampaignSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      case toCardCode attrs of
        "60012" -> do
          owners <- selectList $ IdentityIs Cards.fne60001a
          traverse_ (\you -> cardSpecific attrs "focusMove" you) owners
        "60024" -> removeAccelerationTokenFromPlay
        _ -> pure ()
      pure s
    CardSpecific (isTarget attrs -> True) "focusMove" (fromJSON -> Success you) -> do
      senses <- selectList $ UpgradeWithTrait T.Sense
      upgrades <- filterM (fmap notNull . senseMoveTargets) senses
      chooseOne you $ labeled "$label.done" (cardSpecific attrs "focusPut" you) : [targeting uid do
        moveSenseUpgrade you uid
        cardSpecific attrs "focusMove" you
        | uid <- upgrades]
      pure s
    CardSpecific (isTarget attrs -> True) "focusPut" (fromJSON -> Success you) -> do
      remaining <- identitySenseDeck you
      cards <- filterM (fmap (maybe True notNull) . getPlayTargets you) remaining
      chooseOne you $ labeled "$label.done" (pure ()) : [targeting card do
        cardSpecific you "fneReserveSenseCard" card
        targets <- getPlayTargets you card
        case targets of
          Nothing -> putIntoPlay you card
          Just ts -> chooseOne you [targeting target (putIntoPlayOn you card target) | target <- ts]
        cardSpecific attrs "focusPut" you
        | card <- cards]
      pure s
    _ -> CampaignSideScheme <$> runMessage msg attrs

instance HasModifiersFor CampaignSideScheme where
  getModifiersFor _ target (CampaignSideScheme attrs)
    | isTarget attrs target, toCardCode attrs == "60012" = do
        owner <- selectOne $ IdentityIs Cards.fne60001a
        pure $ maybe [] (\you -> [ThreatRemovableOnlyByIdentity you]) owner
  getModifiersFor _ _ _ = pure []
