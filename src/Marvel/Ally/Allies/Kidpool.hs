module Marvel.Ally.Allies.Kidpool where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Keyword
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Stats ()

kidpool :: AllyCard Kidpool
kidpool = ally Kidpool Cards.kidpool (Thw 1, 0) (Atk 2, 0) (HP 2)

newtype Kidpool = Kidpool (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)


instance HasModifiersFor Kidpool where
  getModifiersFor _ (AllyRef aid) (Kidpool attrs) = pure [KeywordModifier Piercing | aid == allyId attrs]
  getModifiersFor _ _ _ = pure []

instance RunMessage Kidpool where
  runMessage msg (Kidpool attrs) = runQueueT $ case msg of
    _ -> Kidpool <$> runMessage msg attrs
