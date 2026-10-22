module Marvel.Support.Supports.BeatCop (
  beatCop,
  BeatCop (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

beatCop :: SupportCard BeatCop
beatCop = support BeatCop Cards.beatCop

newtype BeatCop = BeatCop (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BeatCop where
  getAbilities (BeatCop a) =
    [ ability a 1 Action (SchemeExists ThwartableScheme) ExhaustCost $ runAbility a 1
    , ability a 2 Action (MinionExists AnyMinion) (ExhaustCost <> DiscardCost (toTarget a)) $
        ChooseDamage (toSource a) (toDamage (supportThreat a) FromAbility) MinionEnemy
    ]

instance RunMessage BeatCop where
  runMessage msg c@(BeatCop attrs) = case msg of
    -- "move 1 threat from a scheme to here"
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      schemes <- selectList ThwartableScheme
      chooseOrRunOne ident
        [ TargetLabel (toRef scheme) [ThwartScheme (toRef scheme) (toSource attrs) 1, Run [cardSpecific attrs "threatMoved" ()]]
        | scheme <- schemes
        ]
      pure c
    CardSpecific (isTarget attrs -> True) "threatMoved" _ ->
      pure $ BeatCop attrs {supportThreat = supportThreat attrs + 1}
    _ -> BeatCop <$> runMessage msg attrs
