module Marvel.Hero.Heroes.ColossusHero where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner

colossusHero :: HeroCard ColossusHero
colossusHero = hero ColossusHero Cards.colossusHero (HP $ Static 14) (HandSize 4) (Thw 1) (Atk 2) (Def 2)

newtype ColossusHero = ColossusHero (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ColossusHero where
  getAbilities (ColossusHero a) =
    [ limitedWindowAbility a 1 (W.IdentityChangedToForm W.After $ IdentityWithId $ heroIdentityId a)
        Response (IsSelf <> CharacterExists (CharacterWithId (IdentityCharacter $ heroIdentityId a) <> CharacterCanGainTough)) NoCost (runAbility a 1)
    ]

instance HasModifiersFor ColossusHero where
  getModifiersFor _ (IdentityRef you) (ColossusHero a) = pure [AdditionalToughStatusCards 1 | you == heroIdentityId a]
  getModifiersFor _ _ _ = pure []

instance RunMessage ColossusHero where
  runMessage msg e@(ColossusHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> e <$ becomeTough you
    _ -> ColossusHero <$> runMessage msg attrs
