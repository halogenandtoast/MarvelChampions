module Marvel.Minion.Minions.ElectroSinister where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Minion.Runner
import Marvel.Window qualified as W
import Marvel.Resource
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Projection
import Marvel.Hand

electroSinister :: MinionCard ElectroSinister
electroSinister = minion ElectroSinister Cards.electroSinister (Sch 1) (Atk 2) (HP 3)

newtype ElectroSinister = ElectroSinister (Attrs Minion)
  deriving anyclass (IsMinion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ElectroSinister where
  getAbilities a@(ElectroSinister attrs) =
    [ windowAbility a 1
        (W.WindowOneOf
          [ W.EngagesMinion W.After AnyIdentity (MinionWithId $ minionId attrs)
          , W.EnemyActivates W.After (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity
          ]) ForcedResponse NoCost (runAbility a 1)
    ]

instance HasModifiersFor ElectroSinister where
  getModifiersFor _ (MinionRef mid) (ElectroSinister attrs) | mid == minionId attrs =
    pure [HitPointModifier $ count (== Energy) $ concatMap (printedResources . getCardDef) $ minionHeldCards attrs]
  getModifiersFor _ _ _ = pure []

instance RunMessage ElectroSinister where
  runMessage msg e@(ElectroSinister attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let you = minionEngagedIdentity attrs
      cards <- filter (elem Energy . printedResources . getCardDef) . unHand <$> project PlayerIdentityHand you
      unless (null cards) $ chooseOne you
        [targeting card (attachPlayerCardToMinion (minionId attrs) card) | card <- cards]
      pure e
    _ -> ElectroSinister <$> runMessage msg attrs
