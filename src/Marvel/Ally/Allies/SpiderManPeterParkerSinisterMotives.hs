module Marvel.Ally.Allies.SpiderManPeterParkerSinisterMotives where

import Marvel.Prelude
import Marvel.Id
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Question ()
import Marvel.Trait (Trait (WebWarrior))
import Marvel.Window qualified as W

spiderManPeterParkerSinisterMotives :: AllyCard SpiderManPeterParkerSinisterMotives
spiderManPeterParkerSinisterMotives = ally SpiderManPeterParkerSinisterMotives Cards.spiderManPeterParkerSinisterMotives (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype SpiderManPeterParkerSinisterMotives = SpiderManPeterParkerSinisterMotives (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderManPeterParkerSinisterMotives where
  getAbilities a@(SpiderManPeterParkerSinisterMotives attrs) =
    [ limitedWindowAbility a 1
        (W.WindowOneOf [W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy, W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme])
        Response (OwnsThis <> CharacterExists (CharacterWithTrait WebWarrior <> CharacterCanBeReadied <> CharacterOtherThan (AllyCharacter $ allyId attrs))) NoCost (runAbility a 1) ]

instance RunMessage SpiderManPeterParkerSinisterMotives where
  runMessage msg a@(SpiderManPeterParkerSinisterMotives attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      characters <- selectList (CharacterWithTrait WebWarrior <> CharacterCanBeReadied)
      let others = filter (/= AllyCharacter (allyId attrs)) characters
      unless (null others) $ chooseOne you
        [ targeting (characterRef character) do
            case character of
              IdentityCharacter ident -> ready ident
              AllyCharacter ally -> ready ally
              _ -> pure ()
        | character <- others ]
      pure a
    _ -> SpiderManPeterParkerSinisterMotives <$> runMessage msg attrs
