module Marvel.Treachery.Treacheries.NextEvolutionSuperStrengthSet where

import Marvel.Upgrade.Types ()
import Marvel.Prelude
import Marvel.Ability ( HasAbilities )
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Keyword qualified as Keyword ()
import Marvel.Matchers
import Marvel.Modifier ( HasModifiersFor )
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.NextEvolution.Sinister ()

import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types


import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype SuperStrengthSetTreachery = SuperStrengthSetTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40158 :: TreacheryCard SuperStrengthSetTreachery
ne40158 = treachery SuperStrengthSetTreachery Cards.ne40158

instance RunMessage SuperStrengthSetTreachery where
  runMessage msg card@(SuperStrengthSetTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      upgrades <- selectList $ UpgradeControlledBy $ IdentityWithId you
      scored <- for upgrades \uid -> do
        def <- getsGame (\g -> abilityCardDef g (UpgradeRef uid))
        pure (uid, maybe 0 (fromMaybe 0 . cdCost) def)
      highest <- selectAny (ActiveVillain <> VillainWithTrait Trait.Psionic)
      let score = (if highest then maximum else minimum) $ map snd scored
          extreme = [uid | (uid, cost) <- scored, cost == score]
      possible <- filterM (\uid -> selectAny $ UpgradeWithId uid <> UpgradeThatCanBeDiscarded) extreme
      if null possible then surge you else chooseOne you [targeting uid (discardUpgrade uid) | uid <- possible]
      pure card
    _ -> SuperStrengthSetTreachery <$> runMessage msg a
