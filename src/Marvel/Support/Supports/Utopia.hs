module Marvel.Support.Supports.Utopia where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Query
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

utopia :: SupportCard Utopia
utopia = support Utopia Cards.utopia

newtype Utopia = Utopia (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Utopia where
  getModifiersFor _ (IdentityRef you) (Utopia attrs) | you == supportController attrs = do
    others <- selectAny (AllyControlledBy (IdentityWithId you) <> NotAlly (AllyWithTrait Trait.XMen))
    pure [AllyLimitModifier 1 | not others]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Utopia where
  getAbilities a =
    [limitedWindowAbility a 1 (W.AllyEntersPlay W.After $ AllyWithTrait Trait.XMen)
      Response (OwnsThis <> CharacterExists (CharacterWithTrait Trait.XMen <> CharacterCanBeReadied))
      ExhaustCost (runAbility a 1)]

instance RunMessage Utopia where
  runMessage msg a@(Utopia attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      characters <- selectList (CharacterWithTrait Trait.XMen <> CharacterCanBeReadied)
      unless (null characters) $ chooseOne you
        [ targeting (characterRef character) $ case character of
            IdentityCharacter ident -> ready ident
            AllyCharacter aid -> ready aid
            _ -> pure ()
        | character <- characters
        ]
      pure a
    _ -> Utopia <$> runMessage msg attrs
