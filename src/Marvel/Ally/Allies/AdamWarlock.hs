module Marvel.Ally.Allies.AdamWarlock (
  adamWarlock,
  AdamWarlock (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Resource
import Marvel.Window hiding (AllyAttacked, AllyThwarted)
import Marvel.Window qualified as W

adamWarlock :: AllyCard AdamWarlock
adamWarlock = ally AdamWarlock Cards.adamWarlock (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype AdamWarlock = AdamWarlock (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef, HasController)

-- | "Response: After Adam Warlock attacks or thwarts, discard 1 card at random from your
-- hand", then a line for each printed resource it has, in printed order (ruling).
instance HasAbilities AdamWarlock where
  getAbilities a@(AdamWarlock attrs) =
    [ limitedWindowAbility
        a
        1
        (WindowOneOf [W.AllyAttacked After self AnyEnemy, W.AllyThwarted After self AnyScheme])
        Response
        OwnsThis
        NoCost
        (runAbility a 1)
    ]
   where
    self = AllyWithId (allyId attrs)

instance RunMessage AdamWarlock where
  runMessage msg a@(AdamWarlock attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      discardFor ident a RandomFromHand 1 1
      pure a
    WithDiscarded (isTarget a -> True) _ (onlyPlayerCards -> cards) -> do
      let printed = concatMap (printedResources . getCardDef) cards
      -- each line its own step: one sees the game after the one before it
      for_ [r | r <- [Physical, Energy, Mental, Wild], r `elem` printed] $ cardSpecific a "resource"
      pure a
    CardSpecific (isTarget a -> True) "resource" (fromJSON -> Success resource) -> do
      let you = controller a
      case resource of
        Physical -> removeThreat you a 3 ThwartableScheme
        Energy -> resolveAs you $ ChooseHeal 3 (IdentityCharacters AnyIdentity)
        Mental -> chooseDamage you a (toDamage 3 FromAbility) AnyEnemy
        Wild ->
          chooseOne you
            [ labeled (countLabel "removeThreatFromAScheme" 3) $ cardSpecific a "resource" Physical
            , labeled (countLabel "healAnIdentity" 3) $ cardSpecific a "resource" Energy
            , labeled (countLabel "dealDamageToAnEnemy" 3) $ cardSpecific a "resource" Mental
            ]
      pure a
    _ -> AdamWarlock <$> runMessage msg attrs
