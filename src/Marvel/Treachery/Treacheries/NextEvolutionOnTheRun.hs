module Marvel.Treachery.Treacheries.NextEvolutionOnTheRun where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Data.List (find)
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Name (display)
import Marvel.NextEvolution.Marauders (canGainStatus)
import Marvel.Query
import Marvel.Status
import Marvel.Trait (Trait (Marauder))
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype OnTheRunTreachery = OnTheRunTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40110 :: TreacheryCard OnTheRunTreachery
ne40110 = treachery OnTheRunTreachery Cards.ne40110

ne40111 :: TreacheryCard OnTheRunTreachery
ne40111 = treachery OnTheRunTreachery Cards.ne40111

instance RunMessage OnTheRunTreachery where
  runMessage msg e@(OnTheRunTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      case toCardCode a of
        "40110" -> do
          heroes <- selectList (IdentityWithId you <> UnexhaustedIdentity)
          allies <- selectList (AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
          unless (null heroes && null allies) $ chooseOne you $
            [targeting ident (exhaust ident) | ident <- heroes]
              <> [targeting (AllyRef aid) (exhaust aid) | aid <- allies]
          cardSpecific a "neDizzyAfterExhaust" you
        "40111" -> do
          minions <- selectList (MinionWithTrait Marauder <> MinionEngagedWith (IdentityWithId you))
          deck <- getEncounterDeck
          let activations = [labeled "$label.neTagTeamActivate" $
                chooseOneAtATime you [targeting mid (minionActivatesAgainst mid you) | mid <- minions] | notNull minions]
              discards = [labeled "$label.neTagTeamDiscard" do
                discardTopOfEncounterDeck 7
                cardSpecific a "neTagTeamTopmost" you | notNull deck]
          unless (null $ activations <> discards) $ chooseOne you (activations <> discards)
        _ -> pure ()
      pure e
    CardSpecific target "neDizzyAfterExhaust" (fromJSON -> Success you) | target == toRef a -> do
      enemies <- selectList AnyEnemy
      titles <- traverse (\enemy -> getsGame $ \g -> maybe "" (display . cdName) (abilityCardDef g $ enemyRef enemy)) enemies
      when ("Arclight" `elem` titles) $ statusControlled you StunnedStatus
      when ("Riptide" `elem` titles) $ dealIndirectDamage you a 3
      when ("Vertigo" `elem` titles) $ statusControlled you ConfusedStatus
      pure e
    CardSpecific target "neTagTeamTopmost" (fromJSON -> Success you) | target == toRef a -> do
      discard <- getEncounterDiscard
      for_ (find (cardMatch $ CardWithType MinionType <> CardWithTrait Marauder) discard) (putEncounterCardIntoPlay you)
      pure e
    _ -> OnTheRunTreachery <$> runMessage msg a

statusControlled :: ReverseQueue m => IdentityId -> StatusCard -> m ()
statusControlled you status = do
  controlled <- selectList (CharacterControlledBy $ IdentityWithId you)
  legal <- filterM (`canGainStatus` status) controlled
  unless (null legal) $ chooseOne you [targeting (characterRef c) (giveStatus c status) | c <- legal]
