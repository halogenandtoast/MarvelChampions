module Marvel.Ally.Allies.SentryRobertReynolds (
  sentryRobertReynolds,
  SentryRobertReynolds (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner hiding (AllyAttacked)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Window

sentryRobertReynolds :: AllyCard SentryRobertReynolds
sentryRobertReynolds = ally SentryRobertReynolds Cards.sentryRobertReynolds (Thw 2, 1) (Atk 3, 1) (HP 5)

newtype SentryRobertReynolds = SentryRobertReynolds (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef, HasController)

-- | "Forced Response: After Sentry enters play under your control, deal yourself 1
-- encounter card."
instance HasAbilities SentryRobertReynolds where
  getAbilities (SentryRobertReynolds a) =
    [ limitedWindowAbility a 1 (PlayThis After) ForcedResponse OwnsThis NoCost $
        Run [DealEncounterCard $ allyController a]
    ]

instance RunMessage SentryRobertReynolds where
  runMessage msg (SentryRobertReynolds attrs) = SentryRobertReynolds <$> runMessage msg attrs
