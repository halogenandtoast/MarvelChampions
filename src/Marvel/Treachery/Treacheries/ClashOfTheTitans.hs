module Marvel.Treachery.Treacheries.ClashOfTheTitans (
  clashOfTheTitans,
  ClashOfTheTitans (..),
) where

import Marvel.Prelude


import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

clashOfTheTitans :: TreacheryCard ClashOfTheTitans
clashOfTheTitans =
  treachery (ClashOfTheTitans . (`With` Meta False)) Cards.clashOfTheTitans

newtype Meta = Meta {attackMade :: Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype ClashOfTheTitans = ClashOfTheTitans (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery ClashOfTheTitans where
  toTreacheryAttrs (ClashOfTheTitans (attrs `With` _)) = attrs

-- | "The enemy with the highest ATK attacks the hero or ally with the highest ATK (first
-- player decides ties.) If no attack was made this way, this card gains surge."
instance RunMessage ClashOfTheTitans where
  runMessage msg t@(ClashOfTheTitans (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery _ -> do
        firstPlayer <- selectJust FirstPlayer
        enemies <- selectList EnemyWithHighestAttack
        characters <- selectList HeroOrAllyWithHighestAttack
        players <- getPlayers
        controllers <- for players \p -> map (,p) <$> selectList (AllyControlledBy $ IdentityWithId p)
        let
          attackedBy enemy = \case
            IdentityCharacter p -> enemyAttacks enemy p
            AllyCharacter aid ->
              for_ (headMay [p | (a, p) <- concat controllers, a == aid]) \p -> enemyAttacksAlly enemy p aid
            _ -> pure ()
        pick firstPlayer enemies enemyRef \enemy ->
          pick firstPlayer characters characterRef (attackedBy enemy)
        pure t
      ResolvedTreachery _ | not (attackMade meta) ->
        ClashOfTheTitans . (`With` meta) <$> runMessage msg (attrs & surgeL .~ True)
      _ -> ClashOfTheTitans . (`With` meta) <$> runMessage msg attrs
    -- a stunned enemy's attack is not made: its stun is removed instead
    VillainMessage _ VillainAttacked -> pure $ ClashOfTheTitans (attrs `With` Meta True)
    MinionMessage _ MinionAttacked -> pure $ ClashOfTheTitans (attrs `With` Meta True)
    _ -> ClashOfTheTitans . (`With` meta) <$> runMessage msg attrs

-- | The one, or the first player decides the tie.
pick :: (ReverseQueue m) => IdentityId -> [x] -> (x -> Target) -> (x -> m ()) -> m ()
pick firstPlayer xs ref f = case xs of
  [x] -> f x
  _ -> chooseOne firstPlayer [targeting (ref x) (f x) | x <- xs]
