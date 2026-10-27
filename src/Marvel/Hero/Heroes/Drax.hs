module Marvel.Hero.Heroes.Drax (
  drax,
  Drax (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

drax :: HeroCard Drax
drax =
  hero
    Drax
    Cards.drax
    (HP $ Static 14)
    (HandSize 4)
    (Thw 1)
    (Atk 1)
    (Def 2)

newtype Drax = Drax (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | "Drax gets +1 ATK for each vengeance counter on him."
instance HasModifiersFor Drax where
  getModifiersFor _ target (Drax a) | isTarget a target = do
    n <- project PlayerIdentityCounters (heroIdentityId a)
    pure [AttackModifier n | n > 0]
  getModifiersFor _ _ _ = pure []

-- | Response: after the villain attacks Drax (ruling: Drax is the attacked character once
-- defenders are declared, undefended or defending himself), place 1 vengeance counter
-- here, to a maximum of 3; if you cannot, draw 1 card.
instance HasAbilities Drax where
  getAbilities (Drax a) =
    [ limitedWindowAbility
        a
        1
        (W.EnemyAttacked W.After (VillainEnemy <> EnemyAttackingCharacter (IdentityCharacters self)) self)
        Response
        IsSelf
        NoCost
        (runAbility a 1)
    ]
   where
    self = IdentityWithId $ heroIdentityId a

instance RunMessage Drax where
  runMessage msg d@(Drax attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      n <- project PlayerIdentityCounters ident
      if n < 3 then placeCounters (IdentityRef ident) 1 else drawCards ident 1
      pure d
    _ -> Drax <$> runMessage msg attrs
