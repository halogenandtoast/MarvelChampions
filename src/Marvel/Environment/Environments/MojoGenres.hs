module Marvel.Environment.Environments.MojoGenres where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype MojoGenreEnvironment = MojoGenreEnvironment (Attrs Environment)
  deriving anyclass IsEnvironment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39035, mojo39041, mojo39047, mojo39053, mojo39060, mojo39066 :: EnvironmentCard MojoGenreEnvironment
mojo39035 = environment MojoGenreEnvironment Cards.mojo39035
mojo39041 = environment MojoGenreEnvironment Cards.mojo39041
mojo39047 = environment MojoGenreEnvironment Cards.mojo39047
mojo39053 = environment MojoGenreEnvironment Cards.mojo39053
mojo39060 = environment MojoGenreEnvironment Cards.mojo39060
mojo39066 = environment MojoGenreEnvironment Cards.mojo39066

instance HasAbilities MojoGenreEnvironment

instance HasModifiersFor MojoGenreEnvironment where
  getModifiersFor _ target (MojoGenreEnvironment a) = case toCardCode a of
    "39035" -> case target of
      IdentityRef _ -> pure [ThwartModifier 1]
      AllyRef _ -> pure [ThwartModifier 1]
      _ -> do
        def <- getsGame (`abilityCardDef` target)
        pure [KeywordModifier $ Incite 1 | maybe False isOtherEncounter def]
    "39041" | IdentityRef _ <- target -> pure [HandSizeModifier 1]
    "39047" -> case target of
      MinionRef _ -> pure [KeywordModifier Quickstrike]
      AllyRef _ -> do
        enemies <- selectList AnyMinion
        pure [AttackConsequentialDamageReductionAgainst (EnemyMinionId mid) 1 | mid <- enemies]
      _ -> pure []
    "39053" -> case target of
      AllyRef _ -> pure [KeywordModifier Toughness]
      MinionRef _ -> pure [KeywordModifier Toughness]
      _ -> pure []
    "39060" | ObligationRef _ <- target -> pure [AccelerationIconModifier 1]
    "39066" -> pure $ [DamageTakenModifier 1 | isCharacter target] <> [KeywordModifier Overkill | isEnemy target]
    _ -> pure []
   where
    isOtherEncounter d = cdCardCode d /= toCardCode a && isJust (cdEncounterSet d)
    isCharacter = \case IdentityRef _ -> True; AllyRef _ -> True; MinionRef _ -> True; VillainRef _ -> True; _ -> False
    isEnemy = \case MinionRef _ -> True; VillainRef _ -> True; _ -> False

instance RunMessage MojoGenreEnvironment where
  runMessage msg e@(MojoGenreEnvironment attrs) = runQueueT $ case msg of
    EnvironmentMessage eid (RevealEnvironment you source) | eid == environmentId attrs -> do
      environments <- selectList (EnvironmentWithTrait Trait.Setting)
      traverse_ removeFromPlay (filter (/= eid) environments)
      when (source == W.RevealedFromEncounterDeck) $ surge you
      pure e
    CardSpecific GameRef "mojoObligationDiscarded" (fromJSON -> Success you) | toCardCode attrs == "39060" -> do
      chooseOne you [labeled "$label.mojoDrawAfterObligation" (drawCards you 1), labeled "$label.done" (pure ())]
      pure e
    _ -> MojoGenreEnvironment <$> runMessage msg attrs
