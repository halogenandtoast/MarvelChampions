module Marvel.Ally.Allies.DaredevilSpdr where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Types
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window qualified as W

daredevilSpdr :: AllyCard DaredevilSpdr
daredevilSpdr = ally DaredevilSpdr Cards.daredevilSpdr (Thw 2, 2) (Atk 1, 1) (HP 3)

newtype DaredevilSpdr = DaredevilSpdr (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DaredevilSpdr where
  getAbilities a@(DaredevilSpdr attrs) =
    [ limitedWindowAbility a 1 (W.AllyDefendsAgainst W.After (AllyWithId $ allyId attrs) (EnemyCanTakeDamageFrom $ toSource attrs))
        Response (OwnsThis <> CharacterExists (AllyCharacters (AllyWithId $ allyId attrs) <> healableCharacter)) NoCost (runAbility a 1)
    | allyDamage attrs > 0 ]

instance RunMessage DaredevilSpdr where
  runMessage msg a@(DaredevilSpdr attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [enemy | W.AllyDefendedAgainst _ enemy <- windows] $ \enemy -> do
        present <- enemyMatches (EnemyCanTakeDamageFrom $ toSource attrs) enemy
        immune <- cannotTakeDamageFrom (enemyRef enemy) (toSource attrs)
        when (present && not immune) do
          heal (allyId attrs) 1
          dealDamage enemy attrs (toDamage 1 FromAbility)
      pure a
    _ -> DaredevilSpdr <$> runMessage msg attrs
