module Marvel.Support.Supports.Citt (
  citt,
  Citt (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Matchers qualified as M
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait (Trait (Guardian))

citt :: SupportCard Citt
citt = support Citt Cards.citt

newtype Citt = Citt (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust C.I.T.T. and spend 2 resources of any type → ready a Guardian
-- character: a hero or alter ego, or an ally, that is exhausted.
instance HasAbilities Citt where
  getAbilities a =
    [ ability a 1 HeroAction (OwnsThis <> exhaustedGuardian) (ExhaustCost <> MultiResourceCost [Nothing, Nothing]) $
        runAbility a 1
    ]
   where
    -- none to ready, no target: it can't be used (RR Target)
    exhaustedGuardian =
      AnyCriteria
        [ CharacterExists (IdentityCharacters (IdentityWithTrait Guardian <> M.ExhaustedIdentity))
        , AllyExists (AllyWithTrait Guardian <> M.ExhaustedAlly)
        ]

instance RunMessage Citt where
  runMessage msg s@(Citt attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      identities <- selectList (IdentityWithTrait Guardian <> M.ExhaustedIdentity)
      allies <- selectList (AllyWithTrait Guardian <> M.ExhaustedAlly)
      chooseOne you $
        [targeting (IdentityRef p) (ready p) | p <- identities]
          <> [targeting (AllyRef x) (ready x) | x <- allies]
      pure s
    _ -> Citt <$> runMessage msg attrs
