module Marvel.Ally.Allies.PatriotRayshaunLucas where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Code ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Modifier ()
import Marvel.Query
import Marvel.Trait
import Marvel.Window qualified as W

patriotRayshaunLucas :: AllyCard PatriotRayshaunLucas
patriotRayshaunLucas = ally PatriotRayshaunLucas Cards.patriotRayshaunLucas (Thw 2, 2) (Atk 1, 1) (HP 4)

newtype PatriotRayshaunLucas = PatriotRayshaunLucas (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PatriotRayshaunLucas where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis HeroResponse OwnsThis NoCost (runAbility a 1)]

instance RunMessage PatriotRayshaunLucas where
  runMessage msg a@(PatriotRayshaunLucas attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      characters <- selectList (CharacterControlledBy AnyIdentity <> CharacterWithTrait Champion)
      chooseOne you
        [ targeting (characterRef character) $ case character of
            IdentityCharacter ident -> createEffect Cards.patriotRayshaunLucas attrs (IdentityEntity $ IdentityWithId ident)
            AllyCharacter ident -> createEffect Cards.patriotRayshaunLucas attrs (AllyEntity $ AllyWithId ident)
            _ -> pure ()
        | character <- characters
        ]
      pure a
    _ -> PatriotRayshaunLucas <$> runMessage msg attrs

newtype PatriotRayshaunLucasEffect = PatriotRayshaunLucasEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

patriotRayshaunLucasEffect :: CardEffect PatriotRayshaunLucasEffect
patriotRayshaunLucasEffect = effectWith PatriotRayshaunLucasEffect Cards.patriotRayshaunLucas $
  (modifiersL .~ [ThwartModifier 1, AttackModifier 1, DefenseModifier 1, RecoveryModifier 1])
    . (endsL ?~ DisableAtEndOfRound)

instance RunMessage PatriotRayshaunLucasEffect where
  runMessage msg (PatriotRayshaunLucasEffect attrs) = PatriotRayshaunLucasEffect <$> runMessage msg attrs
