module Marvel.Minion.Minions.AgeOfApocalypseBlueMoon where

import Marvel.Prelude
import Marvel.Window ()
import Marvel.Question ()
import Marvel.Keyword ()
import Marvel.Damage ()
import Marvel.Criteria ()
import Marvel.Cost ()
import Marvel.Ability
import Marvel.AgeOfApocalypse.Settings
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Matchers qualified as Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Stats
import Marvel.Hp
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait qualified as Trait

newtype AoaBlueMoonMinion = AoaBlueMoonMinion (Attrs Minion `With` Maybe IdentityId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion AoaBlueMoonMinion where
  toMinionAttrs (AoaBlueMoonMinion (attrs `With` _)) = attrs
  overMinionAttrs f (AoaBlueMoonMinion (attrs `With` actor)) = AoaBlueMoonMinion (f attrs `With` actor)

gladiator :: MinionCard AoaBlueMoonMinion
gladiator = minion (AoaBlueMoonMinion . (`With` Nothing)) Cards.gladiator (Sch 2) (Atk 3) (HP 6)

oracle :: MinionCard AoaBlueMoonMinion
oracle = minion (AoaBlueMoonMinion . (`With` Nothing)) Cards.oracle (Sch 2) (Atk 1) (HP 3)

manta :: MinionCard AoaBlueMoonMinion
manta = minion (AoaBlueMoonMinion . (`With` Nothing)) Cards.manta (Sch 1) (Atk 2) (HP 4)

earthquake :: MinionCard AoaBlueMoonMinion
earthquake = minion (AoaBlueMoonMinion . (`With` Nothing)) Cards.earthquake (Sch 2) (Atk 2) (HP 4)

warstar :: MinionCard AoaBlueMoonMinion
warstar = minion (AoaBlueMoonMinion . (`With` Nothing)) Cards.warstar (Sch 1) (Atk 2) (HP 5)

instance HasAbilities AoaBlueMoonMinion

instance HasModifiersFor AoaBlueMoonMinion where
  getModifiersFor _ target (AoaBlueMoonMinion (attrs `With` _))
    | target == toRef attrs, toCardCode attrs == "45140" = do
        trial <- selectAny (SideSchemeIs Schemes.trialByCombat)
        pure [CannotTakeDamage | trial]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaBlueMoonMinion where
  runMessage msg e@(AoaBlueMoonMinion (attrs `With` actor)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      case toCardCode attrs of
        "45141" -> do
          confused <- selectAny (IdentityWithId you <> ConfusedIdentity)
          if confused then resolveSettingSpecial you else confuseIdentity you
        "45142" -> do
          stunned <- selectAny (IdentityWithId you <> StunnedIdentity)
          if stunned then resolveSettingSpecial you else stunIdentity you
        "45143" -> do
          exhausted <- selectAny (IdentityWithId you <> Matchers.ExhaustedIdentity)
          if exhausted then resolveSettingSpecial you else exhaust you
        "45144" -> discardTopOfEncounterDeckFor 1 attrs
        _ -> pure ()
      pure $ AoaBlueMoonMinion (attrs `With` Just you)
    WithDiscarded (isTarget attrs -> True) FromEncounterDeck cards | toCardCode attrs == "45144" -> do
      who <- maybe getActivePlayerId pure actor
      case [card | EncounterCard card <- cards, cardMatch (CardWithType MinionType <> CardWithTrait Trait.ImperialGuard) card] of
        card : _ -> revealEncounterCard who card
        [] -> resolveSettingSpecial who
      pure e
    Boost (RevealedAsBoost target enemy) | target == toRef attrs, toCardCode attrs == "45140" -> do
      trial <- selectAny (SideSchemeIs Schemes.trialByCombat)
      when trial $ getBoostedPlayer enemy >>= dealBoostAsEncounterCard attrs
      pure e
    _ -> AoaBlueMoonMinion . (`With` actor) <$> runMessage msg attrs
