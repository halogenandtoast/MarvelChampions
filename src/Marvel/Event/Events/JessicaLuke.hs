{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.JessicaLuke where
import Marvel.Prelude
import Marvel.Name qualified as Name
import Control.Monad (replicateM_)
import Data.Maybe (maybeToList)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability qualified as Ability
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.JessicaJones.Helpers
import Marvel.Name (Name (..))
import Marvel.Phase
import Marvel.Event.Types
import Marvel.Effect.Types

newtype JLEvent = JLEvent (Attrs Event)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent JLEvent where
  eventAbilitySubTypes (JLEvent a)
    | Name.title (cdName (eventCardDef a)) == "Strategy Session" = do
      hero <- identityMatches HeroIdentity (eventController a)
      pure [Ability.Thwart | hero]
    | otherwise = pure $ cdAbilitySubTypes $ eventCardDef a

jl61004 :: EventCard JLEvent
jl61004 = event JLEvent Cards.jl61004

jl61005 :: EventCard JLEvent
jl61005 = event JLEvent Cards.jl61005

jl61006 :: EventCard JLEvent
jl61006 = event JLEvent Cards.jl61006

jl61019 :: EventCard JLEvent
jl61019 = event JLEvent Cards.jl61019

jl61026 :: EventCard JLEvent
jl61026 = event JLEvent Cards.jl61026

jl61027 :: EventCard JLEvent
jl61027 = event JLEvent Cards.jl61027

jl62003 :: EventCard JLEvent
jl62003 = event JLEvent Cards.jl62003

jl62004 :: EventCard JLEvent
jl62004 = event JLEvent Cards.jl62004

jl62005 :: EventCard JLEvent
jl62005 = event JLEvent Cards.jl62005

jl62006 :: EventCard JLEvent
jl62006 = event JLEvent Cards.jl62006

jl62015 :: EventCard JLEvent
jl62015 = event JLEvent Cards.jl62015

jl62022 :: EventCard JLEvent
jl62022 = event JLEvent Cards.jl62022

jl62023 :: EventCard JLEvent
jl62023 = event JLEvent Cards.jl62023

jl62034 :: EventCard JLEvent
jl62034 = event JLEvent Cards.jl62034

jl62035 :: EventCard JLEvent
jl62035 = event JLEvent Cards.jl62035

instance HasModifiersFor JLEvent where
  getModifiersFor _ (CardIdRef cid) (JLEvent a)
    | Name.title (cdName (eventCardDef a)) == "\"Sweet Christmas!\"", cid == CardId (unEventId $ eventId a) = do
      n <- toughCount (eventController a)
      pure [ResourceCostReduction n]
  getModifiersFor _ _ _ = pure []

instance RunMessage JLEvent where
  runMessage msg e@(JLEvent a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId a -> do
      case Name.title (cdName (eventCardDef a)) of
        "\"Big Mistake\"" -> do
          enemies <- selectList AttackableEnemy
          n <- evidenceCount you
          chooseOne you [targeting (enemyRef enemy) $ chooseOne you $
            [labeled "$label.bigMistakeBase" (dealDamage enemy a $ toDamage 3 $ FromPlayerAttack you)] <>
            (if n > 0 then [labeled "$label.bigMistakePiercing" (spendEvidence you 1 >> dealDamage enemy a (withPiercing $ toDamage 5 $ FromPlayerAttack you)),
             labeled "$label.bigMistakeOverkill" (spendEvidence you 1 >> dealDamage enemy a (withOverkill $ toDamage 5 $ FromPlayerAttack you))] else [])
            | enemy <- enemies]
        "Breakthrough" -> do
          ready you
          afterMatching (== EventMessage eid ResolvedEvent) (shuffleIntoDeck you $ onlyPlayerCards [toCard a])
        "Snooping Around" -> do
          removeThreat you a 4 ThwartableScheme
          addEvidence you 2
          discardTopOfEncounterDeckThen 1 $ \cards ->
            for_ cards $ \card -> when (cdCardType (getCardDef card) `elem` [MinionType, SideSchemeType]) (revealEncounterCardFrom W.RevealedFromDiscardPile you card)
        "Strategy Session" -> do
          hero <- identityMatches HeroIdentity you
          if hero then removeThreat you a 3 (PlayerSideScheme <> ThwartableScheme) else do
            players <- getPlayers
            chooseOne you [targeting p (search p (SearchIdentityDeck p AllOfDeck) (CardWithType PlayerSideSchemeType) SearchDrawOne ShuffleBackIn) | p <- players]
        "Joys of Life" -> for_ (paymentExhaustedCharacters payment) $ \character -> do
          civilian <- characterMatches joysCivilian character
          let destination = if civilian then joysHeroOrAlly else joysCivilian
          targets <- selectList (destination <> CharacterCanBeReadied)
          chooseOne you [targeting (characterRef c) (readyCharacter c) | c <- targets]
        "Unbreakable Bond" -> do
          removeThreat you a 3 ThwartableScheme
          cardSpecific a "healBond" (you, 3 :: Natural)
        "Harlem's Hero" -> becomeTough you >> cardSpecific a "readyForTough" you
        "Knuckle Sandwich" -> do
          chooseDamage you a (toDamage 3 $ FromPlayerAttack you) AttackableEnemy
          cardSpecific a "knuckleReturn" you
        "\"Stand with Me!\"" -> becomeTough you >> cardSpecific a "standThreat" you
        "\"Sweet Christmas!\"" -> do
          minions <- selectList (MinionEngagedWith $ IdentityWithId you)
          withTheVillainIfAny you $ \villain ->
            dealDamageToEach (maybe [] (pure . EnemyVillainId) villain <> map EnemyMinionId minions) a (toDamage 5 $ FromPlayerAttack you)
        "Take a Stand" -> do
          allies <- selectList (AllyControlledBy $ IdentityWithId you)
          for_ allies $ \ally -> createEffect Cards.jl62015 a (AllyEntity $ AllyWithId ally)
        "Dynamic Duo" -> do
          deck <- selectList (InDeckOf (IdentityWithId you) $ BasicCardMatches AnyCard)
          discard <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches AnyCard)
          let teamCards = filter (isJust . cdTeamUp . getCardDef) (deck <> discard)
          when (null teamCards) $ shuffleDeck you
          unless (null teamCards) $ chooseOneSearchedIn you "$label.dynamicDuoTeamUp"
            [("$game.piles.deck", PlayerCard <$> deck), ("$game.piles.discard", PlayerCard <$> discard)]
            [targeting card (addToHand you card >> cardSpecific a "duoAlly" (you, cdTeamUp $ getCardDef card)) | card <- teamCards]
        "Power Man and Iron Fist" -> do
          chooseDamage you a (toDamage 2 $ FromPlayerAttack you) AttackableEnemy
          cardSpecific a "powerManTough" you
        "Size Advantage" -> do
          giant <- identityMatches (IdentityWithTrait T.Giant) you
          yours <- getCharacterRemainingHitPoints (IdentityCharacter you)
          enemies <- selectList AttackableEnemy
          chooseOne you [targeting (enemyRef enemy) $ do
            theirs <- getEnemyRemainingHitPoints enemy
            dealDamage enemy a (toDamage (if giant || yours > theirs then 6 else 3) $ FromPlayerAttack you) | enemy <- enemies]
        "Observe" -> currentVillainSchemes you >> becomeTough you
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "powerManTough" (fromJSON -> Success you) -> do
      characters <- selectList CharacterCanGainTough
      chooseOne you [targeting (characterRef character) (giveStatus character ToughStatus) | character <- characters]
      pure e
    CardSpecific (isTarget a -> True) "readyForTough" (fromJSON -> Success you) -> do
      n <- toughCount you
      cardSpecific a "readyN" (you,n)
      pure e
    CardSpecific (isTarget a -> True) "readyN" (fromJSON -> Success (you :: IdentityId,n :: Natural)) -> do
      when (n > 0) $ do
        targets <- selectList (CharacterControlledBy (IdentityWithId you) <> CharacterCanBeReadied)
        chooseOne you [targeting (characterRef c) (readyCharacter c >> cardSpecific a "readyN" (you,n-1)) | c <- targets]
      pure e
    CardSpecific (isTarget a -> True) "knuckleReturn" (fromJSON -> Success you) -> do
      n <- toughCount you
      when (n > 0) $ chooseOne you
        [ labeled "$label.knuckleSandwichReturn" $ do
            removeStatus you ToughStatus
            afterMatching (== EventMessage (eventId a) ResolvedEvent) (returnToHand a)
        , labeled "$label.done" (pure ())
        ]
      pure e
    CardSpecific (isTarget a -> True) "standThreat" (fromJSON -> Success you) -> do
      n <- min 5 <$> toughCount you
      removeThreat you a n ThwartableScheme
      pure e
    CardSpecific (isTarget a -> True) "duoAlly" (fromJSON -> Success (you :: IdentityId,names :: Maybe (Text,Text))) -> do
      for_ names $ \(first,second) -> do
        deck <- selectList (InDeckOf (IdentityWithId you) $ BasicCardMatches AnyCard)
        discard <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches AnyCard)
        let matches c =
              let def = getCardDef c
                  name = cdName def
               in cdCardType def == AllyType && (title name == first || title name == second || subtitle name `elem` [Just first, Just second])
            fromDeck = filter matches deck
            fromDiscard = filter matches discard
            eligible = fromDeck <> fromDiscard
        unless (null eligible) $ chooseOneSearchedIn you "$label.dynamicDuoAlly"
          [("$game.piles.deck", PlayerCard <$> deck), ("$game.piles.discard", PlayerCard <$> discard)]
          [targeting card (addToHand you card >> shuffleDeck you) | card <- eligible]
        when (null eligible) $ shuffleDeck you
      pure e
    CardSpecific (isTarget a -> True) "healBond" (fromJSON -> Success (you :: IdentityId,n :: Natural)) -> do
      when (n > 0) $ do
        chars <- teamCharacters ["Jessica Jones", "Luke Cage"]
        eligible <- filterM (characterMatches $ CharacterCanBeHealed <> CharacterWithAnyDamage) chars
        chooseOne you [targeting (characterRef c) (heal c 1 >> cardSpecific a "healBond" (you,n-1)) | c <- eligible]
      pure e
    _ -> JLEvent <$> runMessage msg a

-- This response is normally played during the villain phase; its bonuses survive
-- that phase and expire only when the following player phase ends.
newtype TakeAStandEffect = TakeAStandEffect (Attrs Effect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
  deriving anyclass IsEffect

takeAStandEffect :: CardEffect TakeAStandEffect
takeAStandEffect = effectWith TakeAStandEffect Cards.jl62015
  (modifiersL .~ [ThwartModifier 1, AttackModifier 1])

instance RunMessage TakeAStandEffect where
  runMessage msg e@(TakeAStandEffect attrs) = runQueueT $ case msg of
    EndPhase PlayerPhase -> e <$ disableEffect (effectId attrs)
    _ -> TakeAStandEffect <$> runMessage msg attrs
