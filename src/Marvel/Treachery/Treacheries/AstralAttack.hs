module Marvel.Treachery.Treacheries.AstralAttack where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Trait qualified as Trait
import Marvel.EncounterSet

astralAttack :: TreacheryCard AstralAttack
astralAttack = treachery AstralAttack Cards.astralAttack

newtype AstralAttack = AstralAttack (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AstralAttack where
  runMessage msg t@(AstralAttack attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      controlled <- selectList (MinionWithTrait Trait.Controlled)
      if null controlled then gainSurge attrs
      else cardSpecific attrs "chooseActivation" (you, controlled)
      pure t
    CardSpecific (isTarget attrs -> True) "chooseActivation" (fromJSON -> Success (you, remaining :: [MinionId])) -> do
      inGame <- selectAny (IdentityWithId you)
      when inGame do
        present <- filterM (\mid -> selectAny $ MinionWithId mid <> MinionWithTrait Trait.Controlled) remaining
        unless (null present) $ chooseOrRunOne you
          [ targeting (MinionRef mid) do
              cardSpecific attrs "activate" (you, mid)
              cardSpecific attrs "chooseActivation" (you, filter (/= mid) present)
          | mid <- present
          ]
      pure t
    CardSpecific (isTarget attrs -> True) "activate" (fromJSON -> Success (you, mid :: MinionId)) -> do
      inGame <- selectAny (IdentityWithId you)
      present <- selectAny (MinionWithId mid <> MinionWithTrait Trait.Controlled)
      when (inGame && present) do
        hero <- identityMatches HeroIdentity you
        if hero then enemyAttacks mid you else minionSchemesAgainst mid you
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      cards <- filter ((== Just ShadowKingSet) . cdEncounterSet . getCardDef) <$> getEncounterDiscard
      unless (null cards) $ shuffleIntoEncounterDeck cards
      pure t
    _ -> AstralAttack <$> runMessage msg attrs
