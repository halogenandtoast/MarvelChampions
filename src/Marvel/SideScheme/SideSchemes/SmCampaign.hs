module Marvel.SideScheme.SideSchemes.SmCampaign where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Projection
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource (Resource (Physical))
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Window qualified as W

newtype SmCampaignSideScheme = SmCampaignSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smBackAlleyBurglary, smCatInATree, smHenchmenHeist, smOffTheRails, smRubbleRescue :: SideSchemeCard SmCampaignSideScheme
smBackAlleyBurglary = sideSchemeWith SmCampaignSideScheme Cards.smBackAlleyBurglary (PerPlayer 2) (crisisL .~ False)
smCatInATree = sideSchemeWith SmCampaignSideScheme Cards.smCatInATree (PerPlayer 3) (crisisL .~ False)
smHenchmenHeist = sideSchemeWith SmCampaignSideScheme Cards.smHenchmenHeist (PerPlayer 3) (crisisL .~ False)
smOffTheRails = sideSchemeWith SmCampaignSideScheme Cards.smOffTheRails (PerPlayer 3) (crisisL .~ False)
smRubbleRescue = sideSchemeWith SmCampaignSideScheme Cards.smRubbleRescue (PerPlayer 5) (crisisL .~ False)

instance HasModifiersFor SmCampaignSideScheme where
  getModifiersFor _ (SideSchemeRef sid) (SmCampaignSideScheme attrs) | sid == sideSchemeId attrs =
    pure $ case toCardCode attrs of
      "27177" -> [AdditionalThwartCost $ IndirectDamageCost 2]
      "27180" -> [ThwartWithAttack]
      _ -> []
  getModifiersFor _ _ _ = pure []

instance IsSideScheme SmCampaignSideScheme where
  toSideSchemeAttrs (SmCampaignSideScheme attrs) = attrs
  sideSchemeAbilities a@(SmCampaignSideScheme attrs) =
    [ windowAbility a 1 (W.PhaseBegins W.When VillainPhase) ForcedInterrupt NoCost (runAbility a 1)
    | toCardCode attrs == "27179" ]

instance RunMessage SmCampaignSideScheme where
  runMessage msg s@(SmCampaignSideScheme attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "27179" -> do
      placeCounters attrs 1
      when (sideSchemeCounters attrs + 1 >= 2) do
        removeFromGame attrs
        traverse_ (`discardTopOfDeck` 3) =<< getPlayers
      pure s
    SideSchemeMessage sid (SideSchemeThwarted source _) | sid == sideSchemeId attrs -> do
      attrs' <- runMessage msg attrs
      let removed = subtractNatural (sideSchemeThreat attrs') (sideSchemeThreat attrs)
      when (removed > 0) do
        when (toCardCode attrs == "27176") $ do
          you <- maybe getYou pure =<< getSourceController source
          hand <- projectMap PlayerIdentityHand unHand you
          let cost = ResourceCost (Just Physical)
          canPay <- canAffordCost you (toSource attrs) cost
          let choices = [labeled "$label.spendAPhysicalResource" (payForTreachery you cost) | canPay]
                <> [labeled (countLabel "discardRandomCardsFromHand" 1) (discardFrom you RandomFromHand 1) | notNull hand]
          unless (null choices) $ forcedResponse attrs (chooseOne you choices)
        when (toCardCode attrs == "27178") $ putOnMain source removed
      pure $ SmCampaignSideScheme attrs'
    SideSchemeMessage sid (SideSchemeThreatRemoved source _) | sid == sideSchemeId attrs -> do
      attrs' <- runMessage msg attrs
      let removed = subtractNatural (sideSchemeThreat attrs') (sideSchemeThreat attrs)
      when (toCardCode attrs == "27178" && removed > 0) $ putOnMain source removed
      pure $ SmCampaignSideScheme attrs'
    _ -> SmCampaignSideScheme <$> runMessage msg attrs
   where
    putOnMain source removed = do
      you <- maybe getYou pure =<< getSourceController source
      schemes <- selectList EncounterMainScheme
      unless (null schemes) $ forcedResponse attrs $
        chooseOrRunOne you [targeting (schemeRef scheme) (placeThreat scheme removed) | scheme <- schemes]
