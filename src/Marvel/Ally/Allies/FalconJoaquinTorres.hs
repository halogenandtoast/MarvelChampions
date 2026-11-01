module Marvel.Ally.Allies.FalconJoaquinTorres where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Code ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Resource
import Marvel.Trait
import Marvel.Window qualified as W

falconJoaquinTorres :: AllyCard FalconJoaquinTorres
falconJoaquinTorres = ally FalconJoaquinTorres Cards.falconJoaquinTorres (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype FalconJoaquinTorres = FalconJoaquinTorres (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

falconTargets :: Attrs Ally -> CharacterMatcher
falconTargets attrs = CharacterControlledBy (IdentityWithId $ allyController attrs)
  <> CharacterWithTrait Champion <> CharacterOtherThan (AllyCharacter $ allyId attrs) <> CharacterCanBeReadied

instance HasAbilities FalconJoaquinTorres where
  getAbilities a@(FalconJoaquinTorres attrs) =
    [ limitedWindowAbility a 1
        (W.WindowOneOf [W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy, W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme])
        HeroResponse (OwnsThis <> CharacterExists (falconTargets attrs)) (ResourceCost $ Just Energy) (runAbility a 1)
    ]

instance RunMessage FalconJoaquinTorres where
  runMessage msg a@(FalconJoaquinTorres attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      characters <- selectList (falconTargets attrs)
      chooseOne you
        [ targeting (characterRef character) $ case character of
            IdentityCharacter ident -> ready ident
            AllyCharacter ident -> ready ident
            _ -> pure ()
        | character <- characters
        ]
      pure a
    _ -> FalconJoaquinTorres <$> runMessage msg attrs
