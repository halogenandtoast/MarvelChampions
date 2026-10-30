module Marvel.Treachery.Treacheries.ToughAndTumble where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Status
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

-- Only real activations count: a stun/confusion replacement opens no activation window.
data Meta = Meta {chosenEnemies :: [EnemyId], activated :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ToughAndTumble = ToughAndTumble (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery ToughAndTumble where
  toTreacheryAttrs (ToughAndTumble (a `With` _)) = a

toughAndTumble :: TreacheryCard ToughAndTumble
toughAndTumble = treachery (\a -> ToughAndTumble (a `With` Meta [] False)) Cards.toughAndTumble

instance RunMessage ToughAndTumble where
  runMessage msg (ToughAndTumble (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      enemies <- filterM (characterMatches (CharacterWithStatus ToughStatus) . toCharacter) =<< selectList AnyEnemy
      hero <- identityMatches HeroIdentity you
      cardSpecific attrs "activateNext" (you, hero, enemies)
      pure $ ToughAndTumble (attrs `With` Meta enemies False)
    CardSpecific (isTarget attrs -> True) "activateNext" (fromJSON -> Success (you, hero, remaining :: [EnemyId])) -> do
      enemies <- filterM (enemyMatches AnyEnemy) remaining
      let villains = [enemy | enemy@(EnemyVillainId _) <- enemies]
          eligible = if null villains then enemies else villains
      if null eligible then cardSpecific attrs "checkActivations" ()
        else chooseOrRunOne you
          [ targeting (enemyRef enemy) do
              if hero then enemyAttacks enemy you else case enemy of
                EnemyVillainId vid -> villainSchemesAgainst vid you
                EnemyMinionId mid -> minionSchemes mid
              cardSpecific attrs "activateNext" (you, hero, filter (/= enemy) enemies)
          | enemy <- eligible ]
      pure $ ToughAndTumble (attrs `With` meta)
    CheckWindows windows
      | any (\w -> W.windowTiming w == W.When && case W.windowType w of W.EnemyActivation enemy _ -> enemy `elem` chosenEnemies meta; _ -> False) windows ->
          ToughAndTumble . (`With` meta {activated = True}) <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "checkActivations" _ ->
      pure $ ToughAndTumble ((attrs & surgeL .~ not (activated meta)) `With` Meta [] False)
    _ -> ToughAndTumble . (`With` meta) <$> runMessage msg attrs
