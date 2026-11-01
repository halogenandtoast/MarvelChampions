module Marvel.Minion.Minions.Nc48036 (nc48036, Nc48036 (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Modifier ()
import Marvel.Query ()
import Marvel.Ref ()
import Marvel.Window qualified as W

nc48036 :: MinionCard Nc48036
nc48036 = minion (Nc48036 . (`With` Meta False)) Cards.nc48036 (Sch 0) (Atk 2) (HP 4)
newtype Meta = Meta Bool
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
newtype Nc48036 = Nc48036 (Attrs Minion `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance IsMinion Nc48036 where
  toMinionAttrs (Nc48036 (attrs `With` _)) = attrs
  overMinionAttrs f (Nc48036 (attrs `With` meta)) = Nc48036 (f attrs `With` meta)
instance IsRef Nc48036 where
  toRef = toRef . toMinionAttrs
instance HasModifiersFor Nc48036
instance HasAbilities Nc48036 where
  getAbilities a@(Nc48036 (attrs `With` Meta revealedAttack)) =
    [limitedWindowAbility a 1 (W.AllyDefeatedByAttackOf W.When $ EnemyWithId $ EnemyMinionId $ minionId attrs)
      ForcedInterrupt NoCriteria NoCost (runAbility a 1) | revealedAttack]
instance RunMessage Nc48036 where
  runMessage msg a@(Nc48036 (attrs `With` meta)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      characters <- selectList (CharacterControlledBy AnyIdentity)
      remaining <- traverse (\character -> (character,) <$> getCharacterRemainingHitPoints character) characters
      let tied = case remaining of
            [] -> []
            _ -> let lowest = minimum (map snd remaining) in [character | (character,hp) <- remaining, hp == lowest]
      chooseAmongTied (map characterRef tied) $ \target -> case target of
        CharacterRef character -> case character of
          IdentityCharacter ident -> enemyAttacks mid ident
          AllyCharacter aid -> do
            controller <- fromMaybe you <$> getAllyControllerId aid
            enemyAttacksAlly mid controller aid
          _ -> pure ()
        IdentityRef ident -> enemyAttacks mid ident
        AllyRef aid -> do
          controller <- fromMaybe you <$> getAllyControllerId aid
          enemyAttacksAlly mid controller aid
        _ -> pure ()
      pure $ Nc48036 (attrs `With` Meta True)
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [aid | W.DefeatedAlly aid (Just (EnemyMinionId mid)) <- windows, mid == minionId attrs] removeFromGame
      pure a
    MinionMessage mid MinionEndAttack | mid == minionId attrs ->
      Nc48036 . (`With` Meta False) <$> runMessage msg attrs
    _ -> Nc48036 . (`With` meta) <$> runMessage msg attrs
