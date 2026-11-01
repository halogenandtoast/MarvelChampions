module Marvel.Ally.Allies.PipTheTroll (
  pipTheTroll,
  PipTheTroll (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Data.List (find)
import Marvel.Card.Id ()
import Marvel.Card.PlayerCard.Types ()
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Ref ()
import Marvel.Resource
import Marvel.Window

pipTheTroll :: AllyCard PipTheTroll
pipTheTroll = ally PipTheTroll Cards.pipTheTroll (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype PipTheTroll = PipTheTroll (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | The card, as it is in its owner's hand.
pipCard :: Attrs Ally -> CardId
pipCard = CardId . unAllyId . allyId

-- | While Pip the Troll is in your hand, he gains "Interrupt: When a player is attacked,
-- spend [energy][mental] resources → put Pip the Troll into play under that player's
-- control." Sourced on the card in hand, so it is offered only from there; the player
-- attacked is any player whose identity an enemy attacks (R18), before defenders are
-- declared, so Pip may defend.
instance HasAbilities PipTheTroll where
  getAbilities (PipTheTroll a) =
    [ windowAbility
        (CardIdRef (pipCard a) :: Source)
        1
        (EnemyWouldAttack AnyEnemy AnyIdentity)
        Interrupt
        (MultiResourceCost [Just Energy, Just Mental])
        (RunAbility (CardIdRef $ pipCard a) 1)
    ]

instance RunMessage PipTheTroll where
  runMessage msg a@(PipTheTroll attrs) = runQueueT $ case msg of
    RanAbility _ (CardIdRef cid) 1 windows _ | cid == pipCard attrs -> do
      -- he stays his owner's card, under the control of the player attacked
      hand <- projectMap PlayerIdentityHand unHand (allyController attrs)
      for_ (headMay [p | EnemyAttack _ p <- windows]) \attacked ->
        for_ (find ((== cid) . pcCardId) hand) (putIntoPlay attacked)
      pure a
    _ -> PipTheTroll <$> runMessage msg attrs
