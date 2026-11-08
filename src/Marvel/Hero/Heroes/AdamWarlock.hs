module Marvel.Hero.Heroes.AdamWarlock (
  adamWarlockHero,
  adamWarlockEffect,
  AdamWarlockHero (..),
  AdamWarlockEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Aspect
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Damage
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment.Types
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats

adamWarlockHero :: HeroCard AdamWarlockHero
adamWarlockHero =
  hero
    AdamWarlockHero
    Cards.adamWarlockHero
    (HP $ Static 11)
    (HandSize 5)
    (Thw 1)
    (Atk 1)
    (Def 2)

-- | Battle Mage — Action: Discard 1 card from your hand. Limit once per phase. What it
-- does depends on the aspect of the card discarded; a basic card or one of his own does
-- nothing (R21), though the ability has still resolved.
instance HasAbilities AdamWarlockHero where
  getAbilities a@(AdamWarlockHero _) =
    [ label "Battle Mage" $
        limitedAbility a 1 (PerPhase 1) Action IsSelf (DiscardHandCardsCost 1 (Just 1)) (runAbility a 1)
    ]

newtype AdamWarlockHero = AdamWarlockHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage AdamWarlockHero where
  runMessage msg h@(AdamWarlockHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> do
      for_ (paymentDiscards payment) \card -> case cdAspect (getCardDef card) of
        Just Aggression -> chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy
        Just Justice -> removeThreat you attrs 2 ThwartableScheme
        -- any player's ally (R22)
        Just Protection -> do
          allies <- filterM (\aid -> characterMatches healableCharacter (AllyCharacter aid)) =<< selectList (AllyWithDamage $ GreaterThan $ Static 0)
          chooseOne you [targeting (AllyRef ally) (heal ally 1) | ally <- allies]
        Just Leadership -> do
          heroes <- selectList HeroIdentity
          chooseOne you
            [ targeting (IdentityRef ident) $ createEffect Cards.adamWarlockHero attrs (IdentityEntity $ IdentityWithId ident)
            | ident <- heroes
            ]
        _ -> pure ()
      pure h
    _ -> AdamWarlockHero <$> runMessage msg attrs

-- | +1 THW, +1 ATK and +1 DEF this round (Battle Mage, Leadership).
newtype AdamWarlockEffect = AdamWarlockEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

adamWarlockEffect :: CardEffect AdamWarlockEffect
adamWarlockEffect =
  effectWith
    AdamWarlockEffect
    Cards.adamWarlockHero
    ((modifiersL .~ [ThwartModifier 1, AttackModifier 1, DefenseModifier 1]) . (endsL ?~ DisableAtEndOfRound))

instance RunMessage AdamWarlockEffect where
  runMessage msg (AdamWarlockEffect attrs) = AdamWarlockEffect <$> runMessage msg attrs
