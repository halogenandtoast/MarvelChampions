-- | Tower Defense's environment (The Mad Titan's Shadow): Avengers Tower, Stronghold
-- side up, flipped to its Damaged side by damage.
module Marvel.Environment.Environments.AvengersTower (
  avengersTowerStronghold,
  avengersTowerDamaged,
  damageAvengersTower,
  AvengersTower (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Card
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Support.Cards qualified as Supports

avengersTowerStronghold :: EnvironmentCard AvengersTower
avengersTowerStronghold = environment AvengersTower Cards.avengersTowerStronghold

avengersTowerDamaged :: EnvironmentCard AvengersTower
avengersTowerDamaged = environment AvengersTower Cards.avengersTowerDamaged

newtype AvengersTower = AvengersTower (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

isDamaged :: Attrs Environment -> Bool
isDamaged a = toCardCode a == toCardCode Cards.avengersTowerDamaged

-- | "Deal n damage to Avengers Tower": always to the environment, never to an Avengers
-- Tower support (MTS rulebook p.11).
damageAvengersTower :: (ReverseQueue m) => Natural -> m ()
damageAvengersTower n = when (n > 0) do
  stronghold <- selectList (EnvironmentIs Cards.avengersTowerStronghold)
  damaged <- selectList (EnvironmentIs Cards.avengersTowerDamaged)
  for_ (take 1 $ stronghold <> damaged) (`damageEnvironment` n)

-- | Stronghold: "The unique rule does not apply to Avengers Tower."
instance HasModifiersFor AvengersTower where
  getModifiersFor _ GameRef (AvengersTower a) | not (isDamaged a) = pure [UniqueRuleDoesNotApply "Avengers Tower"]
  getModifiersFor _ _ _ = pure []

-- | Forced Response: after damage is placed here, if there is at least 9[per player]
-- damage here, (Stronghold) remove all of it, then flip Avengers Tower over; (Damaged)
-- the players lose. The Damaged side's "When Revealed: Discard each other Avengers Tower
-- from play" is resolved as it is flipped to (ponytail: not a window, as nothing answers
-- "after damage is placed here").
instance RunMessage AvengersTower where
  runMessage msg (AvengersTower attrs) = runQueueT $ case msg of
    EnvironmentMessage ident (EnvironmentDamaged _) | ident == environmentId attrs -> do
      attrs' <- runMessage msg attrs
      limit <- fromIntegral <$> fromGameValue (PerPlayer 9)
      if
        | environmentDamage attrs' < limit -> pure $ AvengersTower attrs'
        | isDamaged attrs -> AvengersTower attrs' <$ gameOver Lost
        | otherwise -> do
            flipEnvironment (environmentId attrs)
            supports <- selectList (SupportNamed (cdName Supports.avengersTower))
            for_ supports removeFromPlay
            pure $ AvengersTower attrs' {environmentDamage = 0}
    _ -> AvengersTower <$> runMessage msg attrs
