module Marvel.Minion.Minions.Nc48027 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Minion.Runner
import Marvel.Minion.Cards qualified as Cards

nc48027 :: MinionCard Nc48027
nc48027 = minion Nc48027 Cards.nc48027 (Sch 2) (Atk 3) (HP 3)

newtype Nc48027 = Nc48027 (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Nc48027 where
  getModifiersFor _ (MinionRef mid) (Nc48027 attrs) = pure [CannotHaveAttachedPlayerCards | mid == minionId attrs]
  getModifiersFor _ _ _ = pure []

instance RunMessage Nc48027 where
  runMessage msg a@(Nc48027 attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      nightcrawler <- selectOne $ IdentityWithTitle "Nightcrawler"
      kurt <- selectOne $ IdentityWithTitle "Kurt Wagner"
      for_ (nightcrawler <|> kurt) $ \you -> dealBoostAsEncounterCard attrs you
      pure a
    _ -> Nc48027 <$> runMessage msg attrs
