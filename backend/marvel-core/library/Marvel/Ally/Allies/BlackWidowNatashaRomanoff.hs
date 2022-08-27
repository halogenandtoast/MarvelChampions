module Marvel.Ally.Allies.BlackWidowNatashaRomanoff
  ( blackWidowNatashaRomanoff
  , BlackWidowNatashaRomanoff(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Resource
import Marvel.Window

blackWidowNatashaRomanoff :: AllyCard BlackWidowNatashaRomanoff
blackWidowNatashaRomanoff = ally
  BlackWidowNatashaRomanoff
  Cards.blackWidowNatashaRomanoff
  (Thw 2, 1)
  (Atk 1, 1)
  (HP 2)

newtype BlackWidowNatashaRomanoff = BlackWidowNatashaRomanoff (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, IsTarget)

instance HasAbilities BlackWidowNatashaRomanoff where
  getAbilities a =
    [ limitedWindowAbility
          a
          1
          (EncounterCardReveal When AnyEncounterCard)
          Interrupt
          NoCriteria
          (ExhaustCost <> ResourceCost (Just Mental))
        $ runAbility a 1
    ]

instance RunMessage BlackWidowNatashaRomanoff where
  runMessage msg a@(BlackWidowNatashaRomanoff attrs) = case msg of
    RanAbility (isTarget a -> True) 1 [EncounterCardRevealed ident _] _ -> do
      replaceMatchingMessage (const [DrawAndRevealEncounterCard ident]) $ \case
        RevealedEncounterCard _ _ -> True
        _ -> False
      pure a
    _ -> BlackWidowNatashaRomanoff <$> runMessage msg attrs
