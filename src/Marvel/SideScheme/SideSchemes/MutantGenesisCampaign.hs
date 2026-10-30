module Marvel.SideScheme.SideSchemes.MutantGenesisCampaign where

import Marvel.Prelude
import Marvel.Ally.Cards qualified as Allies
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Campaign
import Marvel.Card
import Marvel.Deck
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Support.Cards qualified as Supports

newtype MutantGenesisCampaignSideScheme = MutantGenesisCampaignSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mgFrightenedPolice, mgEnemyOfMyEnemy, mgFindThePrisoners, mgSurpriseAttack, mgMagnetosFortress :: SideSchemeCard MutantGenesisCampaignSideScheme
mgFrightenedPolice = sideSchemeWith MutantGenesisCampaignSideScheme Cards.mgFrightenedPolice (PerPlayer 3) (crisisL .~ True)
mgEnemyOfMyEnemy = sideSchemeWith MutantGenesisCampaignSideScheme Cards.mgEnemyOfMyEnemy (PerPlayer 3) (crisisL .~ False)
mgFindThePrisoners = sideSchemeWith MutantGenesisCampaignSideScheme Cards.mgFindThePrisoners (PerPlayer 3) (crisisL .~ False)
mgSurpriseAttack = sideSchemeWith MutantGenesisCampaignSideScheme Cards.mgSurpriseAttack (PerPlayer 3) (crisisL .~ False)
mgMagnetosFortress = sideSchemeWith MutantGenesisCampaignSideScheme Cards.mgMagnetosFortress (PerPlayer 2) (crisisL .~ True)

instance RunMessage MutantGenesisCampaignSideScheme where
  runMessage msg s@(MutantGenesisCampaignSideScheme attrs) = runQueueT $ case msg of
    CardSpecific target "mgHoldCard" (fromJSON -> Success card) | target == toRef attrs ->
      pure $ MutantGenesisCampaignSideScheme attrs {sideSchemeHeldCards = sideSchemeHeldCards attrs <> [card]}
    CardSpecific target "mgClearHeld" _ | target == toRef attrs ->
      pure $ MutantGenesisCampaignSideScheme attrs {sideSchemeHeldCards = []}
    CardSpecific target "mgRemoveHeld" (fromJSON -> Success cid) | target == toRef attrs ->
      pure $ MutantGenesisCampaignSideScheme attrs {sideSchemeHeldCards = filter ((/= (cid :: CardId)) . toCardId) (sideSchemeHeldCards attrs)}
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs, toCardCode attrs == "32173a" -> do
      players <- getPlayers
      for_ players \you -> do
        deck <- filter (cardMatch $ CardWithType AllyType) <$> projectMap PlayerIdentityDeck unDeck you
        unless (null deck) $ chooseOne you [targeting card (placePlayerCardUnder attrs card) | card <- deck]
        shuffleDeck you
      pure s
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      first <- selectJust FirstPlayer
      let you = fromMaybe first (sideSchemeDefeatedBy attrs)
      campaignStep (CampaignSetupThen "mutant:futurePast")
      recordInCampaignLog (RecordMutantSchemeDefeated $ toCardCode attrs)
      case toCardCode attrs of
        "32171a" -> putIntoPlay first =<< playerCard first Supports.mgMetroPD
        "32172a" -> putIntoPlay first =<< playerCard first Allies.mgCampaignMagneto
        "32173a" -> do
          back <- genEncounterCard Environments.mgRescueCaptives
          revealEncounterCard first back
          transferHeldCards attrs (EnvironmentRef $ EnvironmentId $ unCardId $ toCardId back)
        "32174a" -> putObligationIntoPlay you =<< playerCard you Obligations.mgReactiveDefense
        "32175a" -> revealEncounterCard first =<< genEncounterCard Attachments.mgMagnetosPower
        _ -> pure ()
      removeFromGame attrs
      pure s
    _ -> MutantGenesisCampaignSideScheme <$> runMessage msg attrs

playerCard :: MonadRandom m => IdentityId -> CardDef -> m PlayerCard
playerCard you def = do
  cid <- getRandom
  pure $ MkPlayerCard cid def (Just you) (Just you)
