module Marvel.CivilWar.SwitchingSides where

import Marvel.Prelude
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Payment (Payment)
import Marvel.Question (Choice(CardLabel))
import Marvel.Ref
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype SwitchingSides = SwitchingSides (Attrs Treachery `With` Maybe (AllyId,Bool))
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

instance IsTreachery SwitchingSides where
  toTreacheryAttrs (SwitchingSides (a `With` _)) = a

switchingSides :: TreacheryCard SwitchingSides
switchingSides = treachery (\a -> SwitchingSides (a `With` Nothing)) C.cw56158

instance RunMessage SwitchingSides where
  runMessage msg e@(SwitchingSides (a `With` departure)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      cards <- asYou you $ discardableControlledCardsMatching you AnyCard
      let allies = [aid | AllyRef aid <- cards]
      if null allies then gainSurge a else chooseOne you
        [targeting (AllyRef aid) (cardSpecific a "discardAlly" (you,aid)) | aid <- allies]
      pure e
    CardSpecific (isTarget a -> True) "discardAlly" (fromJSON -> Success (you :: IdentityId,aid)) -> do
      card <- getAllyPhysicalCard aid
      discardControlled (AllyRef aid)
      cardSpecific a "afterAllyDeparture" (you,card)
      pure $ SwitchingSides (a `With` Just (aid,False))
    CheckWindows windows -> do
      a' <- runMessage msg a
      let updated = fmap (\(aid,left) -> (aid,left || any (\case
            W.Window W.After (W.AllyLeavingPlay _ other _) -> aid == other
            _ -> False) windows)) departure
      pure $ SwitchingSides (a' `With` updated)
    CardSpecific (isTarget a -> True) "afterAllyDeparture" (fromJSON -> Success (you,card :: Maybe Card)) -> do
      if not (maybe False snd departure) then gainSurge a else do
        competition <- getCompetition
        when (isJust competition) $ for_ card $ \case
          PlayerCard ally | Just printed <- cdCost (getCardDef ally) -> do
            payers <- enemyTeamPlayers you
            first <- enemyTeamChooser you
            let ordered = dropWhile (/= first) payers <> takeWhile (/= first) payers
            cardSpecific a "offerAlly" (ally,fromIntegral (max 0 printed) :: Natural,ordered)
          _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "offerAlly" (fromJSON -> Success (ally,amount,payers :: [IdentityId])) -> do
      case payers of
        [] -> pure ()
        payer : rest -> do
          eligible <- canPutPlayerAllyIntoPlay payer ally
          affordable <- asYou payer $ canAffordCost payer (toSource a) (resourceCost amount)
          if eligible && affordable then chooseOnePromptedAbout payer "$label.cwSwitchingSidesOffer" a
            [fmap (CardLabel $ toCardCode ally) $ labeled (if amount == 0 then "$label.cwSwitchingSidesFree" else countLabel "cwSwitchingSidesPay" amount) $
              payCostThenWithPayment payer a (resourceCost amount) a "allyPaid" (payer,ally)
            ,labeled "$label.pass" (cardSpecific a "offerAlly" (ally,amount,rest))]
          else cardSpecific a "offerAlly" (ally,amount,rest)
      pure e
    CardSpecific (isTarget a -> True) "allyPaid" (fromJSON -> Success ((payer,ally),_payment :: Payment)) -> do
      whenM (canPutPlayerAllyIntoPlay payer ally) $ do
        removePlayerCardFromHeldZones ally
        putIntoPlay payer (ally {pcController = Just payer})
      pure e
    _ -> (\a' -> SwitchingSides (a' `With` departure)) <$> runMessage msg a
   where
    resourceCost 0 = NoCost
    resourceCost n = MultiResourceCost $ replicate (fromIntegral n) Nothing
