module Marvel.Ally.Allies.Nc48012 (nc48012, Nc48012 (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity ()
import Marvel.Id (CharacterId (..))
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted ()
import Marvel.Modifier ()
import Marvel.Nightcrawler.Helpers
import Marvel.Payment ()
import Marvel.Query
import Marvel.Ref ()

nc48012 :: AllyCard Nc48012
nc48012 = ally (Nc48012 . (`With` Meta Nothing)) Cards.nc48012 (Thw 2,1) (Atk 2,1) (HP 3)

newtype Meta = Meta (Maybe CharacterId)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
newtype Nc48012 = Nc48012 (Attrs Ally `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance IsAlly Nc48012 where
  toAllyAttrs (Nc48012 (attrs `With` _)) = attrs
instance IsRef Nc48012 where
  toRef = toRef . toAllyAttrs

-- The chosen character remains a live reference throughout the round (FFG #40,
-- 2024-11-19). Base powers include defined X values per the v1.8 Rogue erratum.
instance HasModifiersFor Nc48012 where
  getModifiersFor _ (AllyRef aid) (Nc48012 (attrs `With` Meta (Just character)))
    | aid == allyId attrs = do
      here <- selectAny (CharacterWithId character)
      if not here then pure [] else do
        (thw, atk) <- basePowers character
        traits <- toList <$> getCharacterTraits character
        pure $ [ThwartModifier thw, AttackModifier atk] <> map TraitModifier traits
  getModifiersFor _ _ _ = pure []

instance HasAbilities Nc48012 where
  getAbilities a@(Nc48012 (attrs `With` _)) = [limitedAbility a 1 (PerRound 1) Action OwnsThis
    (DamageCharacterCost (CharacterControlledBy AnyIdentity <> CharacterOtherThan (AllyCharacter $ allyId attrs)) 1)
    (runAbility a 1)]
instance RunMessage Nc48012 where
  runMessage msg (Nc48012 (attrs `With` meta)) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ payment ->
      pure $ Nc48012 (attrs `With` Meta (headMay [character | ChosenCharacterPayment character <- payments payment]))
    EndRound -> Nc48012 . (`With` Meta Nothing) <$> runMessage msg attrs
    _ -> Nc48012 . (`With` meta) <$> runMessage msg attrs
