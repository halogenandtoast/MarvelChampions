module Marvel.Ally.Allies.ColossusPiotrRasputin where

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
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

colossusPiotrRasputin :: AllyCard ColossusPiotrRasputin
colossusPiotrRasputin = ally ColossusPiotrRasputin Cards.colossusPiotrRasputin (Thw 1, 1) (Atk 3, 2) (HP 3)

newtype ColossusPiotrRasputin = ColossusPiotrRasputin (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ColossusPiotrRasputin where
  getModifiersFor _ (CardIdRef cid) (ColossusPiotrRasputin a) | cid == CardId (unAllyId $ allyId a) = do
    mutant <- identityMatches (IdentityWithTrait Trait.Mutant) (allyController a)
    xmen <- identityMatches (IdentityWithTrait Trait.XMen) (allyController a)
    pure [ResourceCostReduction 1 | mutant || xmen]
  getModifiersFor _ _ _ = pure []

instance RunMessage ColossusPiotrRasputin where
  runMessage msg e@(ColossusPiotrRasputin attrs) = runQueueT $ case msg of
    _ -> ColossusPiotrRasputin <$> runMessage msg attrs
