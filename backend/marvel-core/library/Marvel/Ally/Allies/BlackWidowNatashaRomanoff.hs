module Marvel.Ally.Allies.BlackWidowNatashaRomanoff
  ( blackWidowNatashaRomanoff
  , BlackWidowNatashaRomanoff(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

blackWidowNatashaRomanoff :: AllyCard BlackWidowNatashaRomanoff
blackWidowNatashaRomanoff = ally
  BlackWidowNatashaRomanoff
  Cards.blackWidowNatashaRomanoff
  (Thw 2, 1)
  (Atk 1, 1)
  (HP 2)

newtype BlackWidowNatashaRomanoff = BlackWidowNatashaRomanoff AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities BlackWidowNatashaRomanoff where
  getAbilities (BlackWidowNatashaRomanoff a) =
    [ limitedWindowAbility
          a
          1
          (EncounterCardReveal When AnyEncounterCard)
          Interrupt
          NoCriteria
          (ExhaustCost <> ResourceCost (Just Mental))
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage BlackWidowNatashaRomanoff where
  runMessage msg a@(BlackWidowNatashaRomanoff attrs) = case msg of
    RanAbility target 1 [EncounterCardRevealed ident _]
      | isTarget attrs target -> do
        replaceMatchingMessage [DrawAndRevealEncounterCard ident] $ \case
          RevealedEncounterCard _ _ -> True
          _ -> False
        pure a
    _ -> BlackWidowNatashaRomanoff <$> runMessage msg attrs
