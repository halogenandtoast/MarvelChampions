module Marvel.Ally.Allies.ThorJaneFoster (
  thorJaneFoster,
  ThorJaneFoster (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Def ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Resource
import Marvel.Window

thorJaneFoster :: AllyCard ThorJaneFoster
thorJaneFoster = ally ThorJaneFoster Cards.thorJaneFoster (Thw 1, 1) (Atk 2, 1) (HP 4)

newtype ThorJaneFoster = ThorJaneFoster (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response: After you play Thor from your hand, deal 2 damage to the villain (3 damage
-- instead if you paid for this card using a [physical] resource). Played, not put into
-- play: the window is the card's being played.
instance HasAbilities ThorJaneFoster where
  getAbilities a@(ThorJaneFoster attrs) =
    [ limitedWindowAbility a 1 (PlaysCard After (IdentityWithId $ allyController attrs) (CardIs Cards.thorJaneFoster)) Response OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage ThorJaneFoster where
  runMessage msg a@(ThorJaneFoster attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      physical <- paidWith Physical (allyPaidWith attrs)
      chooseDamage ident a (toDamage (if physical then 3 else 2) FromAbility) VillainEnemy
      pure a
    _ -> ThorJaneFoster <$> runMessage msg attrs
