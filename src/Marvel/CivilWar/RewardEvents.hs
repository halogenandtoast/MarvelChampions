module Marvel.CivilWar.RewardEvents where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as Printed
import Marvel.CivilWar.Helpers
import Marvel.CivilWar.RewardCards qualified as C
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Keyword qualified as K
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Payment
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Resource.Types

newtype LeaderRewardEvent = LeaderRewardEvent (Attrs Event)
  deriving anyclass (IsEvent,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

leaderRewardEvents :: [EventCard LeaderRewardEvent]
leaderRewardEvents = map (event LeaderRewardEvent) C.rewardEvents

instance RunMessage LeaderRewardEvent where
  runMessage msg e@(LeaderRewardEvent a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment window) | eid == eventId a -> do
      friendly <- friendlyLeaderFor you
      enemy <- enemyLeaderFor you
      case toCardCode a of
        "57032" -> for_ window $ \case
          W.ThreatPlaced _ scheme n -> insteadOf (isThreatPlacement scheme) $ placeThreatWithoutWindow scheme (subtractNatural 3 n)
          _ -> pure ()
        "57076" -> do
          attachments <- asYou you $ selectList C.phasedOutAttachments
          unless (null attachments) $ chooseOne you [targeting aid $ removeFromPlay aid | aid <- attachments]
        "57075" -> for_ [(attacker,defender) | Just attacker <- [friendly],Just defender <- [enemy]] $ \(attacker,defender) -> do
          dense <- inAnyArea $ selectAny $ AttachmentOnEnemy (EnemyVillainId attacker) <> AttachmentIs Printed.cw57046a
          unless dense $ flipLeaderMassForm (EnemyVillainId attacker)
          resolvingPlayer <- enemyTeamChooser you
          villainAttacksCharacterWithBonus attacker resolvingPlayer (VillainCharacter defender) (if dense then 1 else 0) [K.Piercing | dense]
        "57074" -> for_ friendly $ \vid -> do
          keys <- getDeckKeys
          let key = if VillainDeckKey vid `elem` keys then VillainDeckKey vid else EncounterDeckKey
          cards <- take 5 . pileDeck <$> getPile key
          recipient <- enemyTeamChooser you
          unless (null cards) $ lookingAt you (map EncounterCard cards) $ chooseOne you
            [cardLabeled (toCardCode card) $ fromDeck key $ dealFoundEncounterCard recipient card | card <- cards]
          fromDeck key shuffleEncounterDeck
        "56129" -> for_ friendly $ \vid -> do
          resolvingPlayer <- enemyTeamChooser you
          previous <- getActivePlayerId
          enemyActivationAfterCurrent vid $ do
            setActivePlayer resolvingPlayer
            villainSchemesWithoutBoostAgainst vid resolvingPlayer
            setActivePlayer previous
        "56209" -> do
          minions <- asYou you $ selectList $ MinionEngagedWith $ IdentityWithId you
          opponents <- enemyTeamPlayers you
          unless (null minions || null opponents) $ chooseOne you [targeting mid $ chooseOne you [targeting opponent $ engageMinion mid opponent | opponent <- opponents] | mid <- minions,not (null opponents)]
        "56211" -> do
          ownSchemes <- getMainSchemesIn =<< getAreaOfTarget you
          for_ (headMay ownSchemes) $ \sid -> removeSchemeThreat a (SchemeMainSchemeId sid) 2
          for_ friendly $ \vid -> do
            schemes <- getMainSchemesIn =<< getAreaOfTarget vid
            for_ (headMay schemes) $ \sid -> placeThreatOn a (SchemeMainSchemeId sid) 2
        _ -> for_ [(attacker,defender) | Just attacker <- [friendly],Just defender <- [enemy]] $ \(attacker,defender) -> do
          leaderAttacksLeaderThen attacker defender $
            cardSpecific a "afterLeaderAttack" (you,defender,payment)
      pure e
    CardSpecific (isTarget a -> True) "afterLeaderAttack" (fromJSON -> Success (you,defender :: VillainId,payment)) -> do
      case toCardCode a of
        "56130" -> whenM (paidWith Mental payment) $ do
          attachments <- asYou you $ selectList $ AttachmentOnEnemy $ EnemyVillainId defender
          eligible <- filterM (\aid -> do
            blanked <- isBlanked aid
            modifiers <- getModifiers aid
            permanent <- getsGame $ \g -> maybe True (elem K.Permanent . cdKeywords) $ abilityCardDef g (AttachmentRef aid)
            abilities <- getsGame $ filter ((== toSource aid) . abilitySource) . getAbilities
            pure $ not blanked && not permanent && all (`notElem` modifiers) [CannotBeDiscarded,CannotLeavePlay,CannotLeavePlayByCardAbilities] && any ((`elem` [HeroResponse,HeroInterrupt]) . abilityType) abilities) attachments
          unless (null eligible) $ chooseOne you [targeting aid $ removeFromPlay aid | aid <- eligible]
        "56134" -> whenM (paidWith Energy payment) $ confuse defender a
        code | code `elem` ["56208","57033"] -> whenM (paidWith Physical payment) $ stun defender a
        "56212" -> whenM (paidWith Mental payment) $ confuse defender a
        _ -> pure ()
      pure e
    _ -> LeaderRewardEvent <$> runMessage msg a
