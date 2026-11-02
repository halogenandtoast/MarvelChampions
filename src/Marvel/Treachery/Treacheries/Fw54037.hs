{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Treachery.Treacheries.Fw54037 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Status (StatusCard(StunnedStatus))
import Marvel.Hp
import Marvel.Id
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
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Treachery.Types

import Marvel.Hood.Helpers (activationWillResolve)
import Marvel.FalconWinter.Helpers
fw54037 :: TreacheryCard Fw54037
fw54037 = treachery Fw54037 Cards.fw54037

newtype Fw54037 = Fw54037 (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw54037 where
  runMessage msg a@(Fw54037 attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      findAndActivateMinion you attrs Cards.fw54034
      pure a
    CardSpecific (isTarget attrs -> True) "fwActivateFoundMinion" (fromJSON -> Success (you, def :: CardDef)) -> do
      found <- selectOne $ MinionIs def
      case found of
        Nothing -> pure $ Fw54037 attrs {treacherySurge = True}
        Just mid -> do
          resolves <- activationWillResolve you (EnemyMinionId mid)
          activateEnemy (EnemyMinionId mid) you
          unless resolves $ gainSurge attrs
          pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      already <- characterHasCondition StunnedStatus (IdentityCharacter you)
      stunIdentity you
      when already $ damageIdentity you attrs (toDamage 1 FromAbility)
      pure a
    _ -> Fw54037 <$> runMessage msg attrs
