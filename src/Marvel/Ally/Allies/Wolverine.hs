module Marvel.Ally.Allies.Wolverine where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Keyword

wolverine :: AllyCard Wolverine
wolverine = ally Wolverine Cards.wolverine (Thw 1, 1) (Atk 3, 2) (HP 4)

newtype Wolverine = Wolverine (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Wolverine where
  getAbilities (Wolverine a) =
    [limitedWindowAbility a 1 (W.TurnBegins $ IdentityWithId $ allyController a) Response (OwnsThis <> CharacterExists (AllyCharacters (AllyWithId $ allyId a) <> healableCharacter)) NoCost (runAbility a 1)]

instance HasModifiersFor Wolverine where
  getModifiersFor _ (AllyRef aid) (Wolverine a) = pure [KeywordModifier Piercing | aid == allyId a]
  getModifiersFor _ _ _ = pure []

instance RunMessage Wolverine where
  runMessage msg e@(Wolverine attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ heal (allyId attrs) 1
    _ -> Wolverine <$> runMessage msg attrs
