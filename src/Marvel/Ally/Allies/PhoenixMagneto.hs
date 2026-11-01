module Marvel.Ally.Allies.PhoenixMagneto where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id (CharacterId(AllyCharacter))
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

phoenixMagneto :: AllyCard PhoenixMagneto
phoenixMagneto = ally PhoenixMagneto Cards.phoenixMagneto (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype PhoenixMagneto = PhoenixMagneto (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PhoenixMagneto where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> AnyCriteria [CharacterExists (AllyCharacters (AllyWithTrait T.XMen) <> state) | state <- [CharacterWithAnyDamage <> CharacterCanBeHealed, CharacterCanBeReadied]]) NoCost (runAbility a 1)]

instance RunMessage PhoenixMagneto where
  runMessage msg e@(PhoenixMagneto attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      allies <- filterM (\aid -> anyM (\state -> selectAny (CharacterWithId (AllyCharacter aid) <> state)) [CharacterWithAnyDamage <> CharacterCanBeHealed, CharacterCanBeReadied]) =<< selectList (AllyWithTrait T.XMen)
      chooseOne you [targeting aid $ ready aid >> heal aid 1 | aid <- allies]
      pure e
    _ -> PhoenixMagneto <$> runMessage msg attrs
