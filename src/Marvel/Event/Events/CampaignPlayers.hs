{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.CampaignPlayers where
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
import Marvel.Event.Types

cw56008 :: EventCard CampaignEvent
cw56008 = event CampaignEvent Cards.cw56008

cw56009 :: EventCard CampaignEvent
cw56009 = event CampaignEvent Cards.cw56009

cw56016 :: EventCard CampaignEvent
cw56016 = event CampaignEvent Cards.cw56016

cw56017 :: EventCard CampaignEvent
cw56017 = event CampaignEvent Cards.cw56017

cw56037 :: EventCard CampaignEvent
cw56037 = event CampaignEvent Cards.cw56037

cw56038 :: EventCard CampaignEvent
cw56038 = event CampaignEvent Cards.cw56038

cw56039 :: EventCard CampaignEvent
cw56039 = event CampaignEvent Cards.cw56039

cw56040 :: EventCard CampaignEvent
cw56040 = event CampaignEvent Cards.cw56040

cw56047 :: EventCard CampaignEvent
cw56047 = event CampaignEvent Cards.cw56047

cw56050 :: EventCard CampaignEvent
cw56050 = event CampaignEvent Cards.cw56050

fne60008 :: EventCard CampaignEvent
fne60008 = event CampaignEvent Cards.fne60008

fne60009 :: EventCard CampaignEvent
fne60009 = event CampaignEvent Cards.fne60009

fne60010 :: EventCard CampaignEvent
fne60010 = event CampaignEvent Cards.fne60010

fne60011 :: EventCard CampaignEvent
fne60011 = event CampaignEvent Cards.fne60011

fne60023 :: EventCard CampaignEvent
fne60023 = event CampaignEvent Cards.fne60023

fne60040a :: EventCard CampaignEvent
fne60040a = event CampaignEvent Cards.fne60040a

fne60040b :: EventCard CampaignEvent
fne60040b = event CampaignEvent Cards.fne60040b

fne60040c :: EventCard CampaignEvent
fne60040c = event CampaignEvent Cards.fne60040c

fne60041 :: EventCard CampaignEvent
fne60041 = event CampaignEvent Cards.fne60041

fne60048 :: EventCard CampaignEvent
fne60048 = event CampaignEvent Cards.fne60048

fne60049 :: EventCard CampaignEvent
fne60049 = event CampaignEvent Cards.fne60049

fne60050 :: EventCard CampaignEvent
fne60050 = event CampaignEvent Cards.fne60050

fne60055 :: EventCard CampaignEvent
fne60055 = event CampaignEvent Cards.fne60055

newtype CampaignEvent = CampaignEvent (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent CampaignEvent

instance RunMessage CampaignEvent where
  runMessage msg e@(CampaignEvent attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ windows) | eid == eventId attrs -> do
      case toCardCode attrs of
        "56008" -> do
          chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) AttackableEnemy
          cardSpecific attrs "toothSecond" you
        "56009" -> do
          schemes <- filterM (canRemoveThreatFrom $ toSource attrs) =<< selectList ThwartableScheme
          chooseOne you [targeting (schemeTarget scheme) do
            removeThreatFrom scheme attrs 3
            cardSpecific attrs "threatRemovalDone" scheme
            | scheme <- schemes]
        "56016" -> do
          minions <- selectList $ NotMinion $ MinionWithTrait T.Elite
          eligible <- filterM (\mid -> enemyMatches (EnemyWithUpgrade (UpgradeMatches [])) (EnemyMinionId mid)) minions
          chooseOne you [targeting mid (defeatMinionIgnoringWhenDefeated you attrs mid) | mid <- eligible]
        "56017" -> do
          addToBasicPower you 6
          addKeywordToAttack you attrs K.Piercing
        "56037" -> do
          chooseDamage you attrs (toDamage 5 $ FromPlayerAttack you) AttackableEnemy
          cardSpecific attrs "readyShape" you
        "56038" -> do
          defense <- identityDefense you
          for_ (windowEnemies windows) $ \enemy -> do
            dealDamage enemy attrs (toDamage defense FromAbility)
            afterEnemyAttack enemy $ giveStatus you ToughStatus
        "56039" -> do
          schemes <- filterM (canRemoveThreatFrom $ toSource attrs) =<< selectList ThwartableScheme
          chooseOne you [targeting (schemeTarget scheme) do
            removeThreatFrom scheme attrs 4
            cardSpecific attrs "threatRemovalDone" scheme
            | scheme <- schemes]
        "56040" -> searchOwned you attrs (CardWithTrait T.Shapeshift) True
        "56047" -> for_ (windowEnemies windows) $ \enemy -> declareHeroDefenseWithoutExhausting you enemy 0
        "56050" -> lastingModifiers attrs (IdentityEntity $ IdentityWithId you) [KeywordModifier $ K.Retaliate 1] False
        "60008" -> do
          enemies <- selectList AttackableEnemy
          chooseOne you [targeting (enemyRef enemy) do
            n <- upgradesOnEnemy enemy
            chooseOne you [labeled (countLabel "dealDamageToAnEnemy" $ 3 + bonus) (dealDamage enemy attrs $ toDamage (3 + bonus) $ FromPlayerAttack you) | bonus <- [0 .. n]]
            | enemy <- enemies]
        "60009" -> do
          chooseSense you attrs True
          cardSpecific attrs "depositionThwart" you
        "60010" -> do
          schemes <- selectList ThwartableScheme
          chooseOne you [targeting (schemeTarget scheme) do
            n <- upgradesOnScheme scheme
            chooseOne you [labeled (countLabel "removeThreatFromAScheme" $ 2 + bonus) (removeThreatFrom scheme attrs $ 2 + bonus) | bonus <- [0 .. n]]
            | scheme <- schemes]
        "60011" -> do
          aerial <- identityMatches (IdentityWithTrait T.Aerial) you
          enemies <- selectList AttackableEnemy
          damages <- for enemies $ \enemy -> do
            n <- upgradesOnEnemy enemy
            pure (enemy, toDamage (n * if aerial then 3 else 2) $ FromPlayerAttack you)
          dealDamagesToEach attrs damages
        "60023" -> do
          removeThreat you attrs 1 ThwartableScheme
          cardSpecific attrs "knowEnemySecond" you
        "60041" -> do
          players <- selectList AnyIdentity
          cards <- concatMapM (projectMap PlayerIdentityDiscard unDiscard) players
          let eligible = filter (\card -> cardMatch (CardWithType EventType) card && (isJust (cdAspect $ getCardDef card) || toCardCode card `elem` ["60040a","60040b","60040c"])) cards
          chooseOne you [targeting card (removeFromGame card >> addToHand you card) | card <- eligible]
        "60048" -> ready you
        "60049" -> removeThreat you attrs 3 ThwartableScheme
        "60050" -> do
          defense <- identityDefense you
          chooseDamage you attrs (toDamage defense $ FromPlayerAttack you) AttackableEnemy
          cardSpecific attrs "harmThwart" (you, defense)
        "60055" -> cardSpecific attrs "seeHear" (you, 2 :: Natural)
        _ -> pure ()
      pure e
    CheckWindows windows | toCardCode attrs `elem` ["56009","56039"] -> do
      for_ [scheme | W.Window W.After (W.ThreatRemovedBy source scheme before removed) <- windows, source == toSource attrs, before > 0, removed == before] $ \scheme ->
        insteadOf (matchesRemovalDone scheme) $
          cardSpecific attrs (if toCardCode attrs == "56009" then "felineStun" else "impersonationFlip") (eventController attrs)
      CampaignEvent <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "threatRemovalDone" _ -> pure e
    CardSpecific (isTarget attrs -> True) "toothSecond" (fromJSON -> Success you) -> e <$ chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) AttackableEnemy
    CardSpecific (isTarget attrs -> True) "felineStun" (fromJSON -> Success you) -> e <$ chooseEnemyStatus you StunnedStatus MinionEnemy
    CardSpecific (isTarget attrs -> True) "readyShape" (fromJSON -> Success you) -> do
      upgrades <- selectList $ UpgradeControlledBy (IdentityWithId you) <> UpgradeWithTrait T.Shapeshift <> UpgradeCanBeReadied
      unless (null upgrades) $ chooseOrRunOne you [targeting uid (ready uid) | uid <- upgrades]
      pure e
    CardSpecific (isTarget attrs -> True) "impersonationFlip" (fromJSON -> Success you) -> e <$ chooseOne you [labeled "$label.changeToB" (changeToAlterEgo you), labeled "$label.done" (pure ())]
    CardSpecific (isTarget attrs -> True) "depositionThwart" (fromJSON -> Success you) -> e <$ removeThreat you attrs 2 ThwartableScheme
    CardSpecific (isTarget attrs -> True) "knowEnemySecond" (fromJSON -> Success you) -> e <$ removeThreat you attrs 1 ThwartableScheme
    CardSpecific (isTarget attrs -> True) "harmThwart" (fromJSON -> Success (you,n)) -> e <$ removeThreat you attrs n ThwartableScheme
    CardSpecific (isTarget attrs -> True) "seeHear" (fromJSON -> Success (you,n :: Natural)) -> do
      when (n > 0) $ chooseOne you
        [labeled "$label.dealDamage" (chooseDamage you attrs (toDamage 3 $ FromPlayerAttack you) AttackableEnemy >> cardSpecific attrs "seeHear" (you,n-1))
        ,labeled "$label.removeThreat" (removeThreat you attrs 3 ThwartableScheme >> cardSpecific attrs "seeHear" (you,n-1))]
      pure e
    _ -> CampaignEvent <$> runMessage msg attrs

   where
    matchesRemovalDone scheme = \case
      CardSpecific (isTarget attrs -> True) "threatRemovalDone" (fromJSON -> Success (targetScheme :: SchemeId)) -> scheme == targetScheme
      _ -> False
