module Marvel.Ally.Allies.PsylockeAngel where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Matchers qualified as Matchers
import Marvel.Query ()
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

psylockeAngel :: AllyCard PsylockeAngel
psylockeAngel = ally PsylockeAngel Cards.psylockeAngel (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype PsylockeAngel = PsylockeAngel (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PsylockeAngel where
  getAbilities a@(PsylockeAngel attrs) =
    [limitedWindowAbility a 1 (W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy)
      HeroResponse (OwnsThis <> AnyCriteria
        [ SelfMatches (IdentityWithTitle "Angel") <> AllyExists (AllyWithId (allyId attrs) <> AllyWithDamage (GreaterThan (Static 0)))
            <> CharacterExists (CharacterWithId (AllyCharacter $ allyId attrs) <> CharacterCanBeHealed)
        , SelfMatches (IdentityWithTitle "Archangel") <> SelfMatches Matchers.ExhaustedIdentity
        ]) NoCost (runAbility a 1)]

instance RunMessage PsylockeAngel where
  runMessage msg a@(PsylockeAngel attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      angel <- identityMatches (IdentityWithTitle "Angel") you
      archangel <- identityMatches (IdentityWithTitle "Archangel") you
      when angel $ heal (allyId attrs) 1
      when archangel $ ready you
      pure a
    _ -> PsylockeAngel <$> runMessage msg attrs
