module Marvel.Treachery.Treacheries.ClashOfTheTitans (
  clashOfTheTitans,
  ClashOfTheTitans (..),
) where

import Marvel.Prelude

import Data.Maybe (maybeToList)

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
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
  runMessage msg t@(ClashOfTheTitans (attrs `With` meta)) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery _ -> do
        firstPlayer <- selectJust FirstPlayer
        enemies <- selectList EnemyWithHighestAttack
        characters <- selectList HeroOrAllyWithHighestAttack
        players <- getPlayers
        controllers <- for players \p -> map (,p) <$> selectList (AllyControlledBy $ IdentityWithId p)
        let
          attackedBy enemy = \case
            IdentityCharacter p -> Just $ enemyMessage enemy (VillainAttacks p) (MinionAttacks p)
            AllyCharacter aid -> do
              p <- headMay [p | (a, p) <- concat controllers, a == aid]
              Just $ enemyMessage enemy (VillainAttacksAlly p aid) (MinionAttacksAlly p aid)
            _ -> Nothing
          -- the first player decides ties
          pick :: [x] -> (x -> Target) -> (x -> [Message]) -> [Message]
          pick xs ref f = case xs of
            [x] -> f x
            _ -> [Ask firstPlayer $ ChooseOne [TargetLabel (ref x) [Run (f x)] | x <- xs]]
        pushAll $
          pick enemies enemyRef \enemy ->
            pick characters characterRef \character -> maybeToList (attackedBy enemy character)
        pure t
      ResolvedTreachery _ | not (attackMade meta) ->
        ClashOfTheTitans . (`With` meta) <$> runMessage msg (attrs & surgeL .~ True)
      _ -> ClashOfTheTitans . (`With` meta) <$> runMessage msg attrs
    -- a stunned enemy's attack is not made: its stun is removed instead
    VillainMessage _ VillainAttacked -> pure $ ClashOfTheTitans (attrs `With` Meta True)
    MinionMessage _ MinionAttacked -> pure $ ClashOfTheTitans (attrs `With` Meta True)
    _ -> ClashOfTheTitans . (`With` meta) <$> runMessage msg attrs
   where
    enemyMessage enemy villainMsg minionMsg = case enemy of
      EnemyVillainId vid -> VillainMessage vid villainMsg
      EnemyMinionId mid -> MinionMessage mid minionMsg
    enemyRef = \case
      EnemyVillainId vid -> toRef vid
      EnemyMinionId mid -> toRef mid
    characterRef = \case
      IdentityCharacter p -> toRef p
      AllyCharacter aid -> toRef aid
      other -> toRef other
