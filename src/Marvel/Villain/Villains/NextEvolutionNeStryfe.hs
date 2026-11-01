module Marvel.Villain.Villains.NextEvolutionNeStryfe where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Ability
import Marvel.Question (runAbility)
import Marvel.Cost
import Marvel.Damage
import Marvel.Deck
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Id
import Marvel.NextEvolution.Stryfe
import Marvel.Query
import Marvel.Trait (Trait (Psionic))
import Marvel.Window qualified as W
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NeStryfeVillain = NeStryfeVillain (Attrs Villain `With` Maybe IdentityId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

ne40163 :: VillainCard NeStryfeVillain
ne40163 = villainWith (\a -> NeStryfeVillain (a `With` Nothing)) Cards.ne40163 (Sch 1) (Atk 0) (HP $ PerPlayer 15) (stageL .~ 1)

ne40164 :: VillainCard NeStryfeVillain
ne40164 = villainWith (\a -> NeStryfeVillain (a `With` Nothing)) Cards.ne40164 (Sch 1) (Atk 1) (HP $ PerPlayer 17) (stageL .~ 2)

ne40165 :: VillainCard NeStryfeVillain
ne40165 = villainWith (\a -> NeStryfeVillain (a `With` Nothing)) Cards.ne40165 (Sch 2) (Atk 1) (HP $ PerPlayer 20) (stageL .~ 3)

instance IsVillain NeStryfeVillain where
  toVillainAttrs (NeStryfeVillain (a `With` _)) = a
  overVillainAttrs f (NeStryfeVillain (a `With` who)) = NeStryfeVillain (f a `With` who)

instance HasModifiersFor NeStryfeVillain where
  getModifiersFor _ target (NeStryfeVillain (a `With` _)) | target == toRef a, isJust (villainAttacking a) = do
    case villainActivationPlayer a of
      Just you -> pure . pure . AttackModifier =<< handTypeCount you
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities NeStryfeVillain where
  getAbilities (NeStryfeVillain (a `With` _)) =
    [windowAbility a 2 (W.VillainRevealed W.When (VillainWithId $ villainId a) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 2) | toCardCode (getCardDef a) == "40164"] <>
    [windowAbility a 1 (W.IdentityAttacked W.After AnyIdentity (EnemyWithId $ toEnemyId a)) ForcedResponse NoCost (runAbility a 1) | toCardCode (getCardDef a) == "40165"]

instance RunMessage NeStryfeVillain where
  runMessage msg e@(NeStryfeVillain (a `With` who)) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      n <- handTypeCount you
      damageIdentity you a (toDamage n FromAbility)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      players <- selectList AnyIdentity
      for_ players $ \you -> cardSpecific a "neStryfeFindPsionic" you
      pure e
    CardSpecific target "neStryfeFindPsionic" (fromJSON -> Success you) | target == toRef a -> do
      discardUntil FromEncounterDeck (CardWithTrait Psionic <> CardWithType AttachmentType) a
      pure $ NeStryfeVillain (a `With` Just you)
    WithDiscardedMatch target FromEncounterDeck (EncounterCard card) | target == toRef a -> do
      -- Each search is queued separately, so its reveal resolves before the next search.
      you <- maybe (selectJust FirstPlayer) pure who
      revealEncounterCard you card
      pure e
    _ -> (\attrs -> NeStryfeVillain (attrs `With` who)) <$> runMessage msg a
