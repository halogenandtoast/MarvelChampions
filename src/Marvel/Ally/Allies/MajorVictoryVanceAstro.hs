module Marvel.Ally.Allies.MajorVictoryVanceAstro (
  majorVictoryVanceAstro,
  MajorVictoryVanceAstro (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Matchers qualified as Matchers
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Guardian))
import Marvel.Window qualified as W

majorVictoryVanceAstro :: AllyCard MajorVictoryVanceAstro
majorVictoryVanceAstro = ally MajorVictoryVanceAstro Cards.majorVictoryVanceAstro (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype MajorVictoryVanceAstro = MajorVictoryVanceAstro (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Interrupt: When Major Victory is defeated, choose a friendly Guardian character →
-- ready that character: an exhausted one, any player's identity or ally but his own
-- self, leaving play (RR Target).
instance HasAbilities MajorVictoryVanceAstro where
  getAbilities (MajorVictoryVanceAstro a) =
    [ limitedWindowAbility
        a
        1
        (W.AllyDefeated W.When $ AllyWithId $ allyId a)
        Interrupt
        (OwnsThis <> AnyCriteria [AllyExists (others a), CharacterExists (IdentityCharacters guardianIdentities)])
        NoCost
        (runAbility a 1)
    ]

others :: Attrs Ally -> AllyMatcher
others a = Matchers.ExhaustedAlly <> AllyWithTrait Guardian <> NotAlly (AllyWithId $ allyId a)

guardianIdentities :: IdentityMatcher
guardianIdentities = Matchers.ExhaustedIdentity <> IdentityWithTrait Guardian

instance RunMessage MajorVictoryVanceAstro where
  runMessage msg a@(MajorVictoryVanceAstro attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      identities <- selectList guardianIdentities
      allies <- selectList (others attrs)
      chooseOne you $
        [targeting (IdentityRef i) (ready i) | i <- identities]
          <> [targeting (AllyRef ally') (ready ally') | ally' <- allies]
      pure a
    _ -> MajorVictoryVanceAstro <$> runMessage msg attrs
