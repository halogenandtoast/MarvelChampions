module Marvel.Treachery.Treacheries.NextEvolutionMorlockSiege where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Marvel.Cost
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.NextEvolution.Helpers
import Marvel.Query
import Marvel.Status
import Marvel.Trait (Trait (Morlock))
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MorlockSiegeTreachery = MorlockSiegeTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40080 :: TreacheryCard MorlockSiegeTreachery
ne40080 = treachery MorlockSiegeTreachery Cards.ne40080

ne40088 :: TreacheryCard MorlockSiegeTreachery
ne40088 = treachery MorlockSiegeTreachery Cards.ne40088

ne40089 :: TreacheryCard MorlockSiegeTreachery
ne40089 = treachery MorlockSiegeTreachery Cards.ne40089

instance RunMessage MorlockSiegeTreachery where
  runMessage msg e@(MorlockSiegeTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      count <- routedCount
      case toCardCode a of
        "40080" -> giveMorlockTough you
        "40088" -> do
          selectOne ActiveVillain >>= traverse_ enemyBecomesTough
          placeThreatOnEach a count EncounterMainScheme
        "40089" -> do
          hero <- selectAny (IdentityWithId you <> HeroIdentity)
          if not hero then surge you else do
            vid <- selectOne ActiveVillain
            for_ vid \v -> enemyAttacksGaining v you [Overkill | count > 0]
        _ -> pure ()
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "40080" -> giveMorlockTough you >> dealBoost enemy
        "40088" -> do
          morlocks <- selectList (AllyControlledBy (IdentityWithId you) <> AllyWithTrait Morlock)
          unless (null morlocks) do
            payable <- asYou you $ canAffordCost you (toSource a) (ResourceCost Nothing)
            chooseOne you $
              [targeting (AllyRef aid) (damageCharacter aid a $ toDamage 1 FromAbility) | aid <- morlocks]
                <> [labeled "$label.neSpendOneResource" (payForTreachery you $ ResourceCost Nothing) | payable]
        "40089" -> do
          attacking <- isJust <$> getEnemyAttack enemy
          when attacking $ attackGains enemy Overkill
        _ -> pure ()
      pure e
    _ -> MorlockSiegeTreachery <$> runMessage msg a

giveMorlockTough :: ReverseQueue m => IdentityId -> m ()
giveMorlockTough you = do
  morlocks <- selectList (CharacterWithTrait Morlock <> CharacterCanGainTough)
  unless (null morlocks) $ chooseOne you [targeting (characterRef c) (giveStatus c ToughStatus) | c <- morlocks]
