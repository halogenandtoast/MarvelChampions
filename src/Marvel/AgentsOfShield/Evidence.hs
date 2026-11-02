{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.AgentsOfShield.Evidence where

import Marvel.Prelude

-- Evidence is campaign information, not an encounter-deck card.
data EvidenceKind = Means | Motive | Opportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Evidence = Evidence { evidenceCode :: Text, evidenceName :: Text, evidenceKind :: EvidenceKind }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

evidence :: [Evidence]
evidence =
  [ Evidence "50185" "Medical Records" Means
  , Evidence "50186" "Wiretap" Means
  , Evidence "50187" "Security Scanner" Means
  , Evidence "50188" "Money" Motive
  , Evidence "50189" "Blackmail" Motive
  , Evidence "50190" "Ideology" Motive
  , Evidence "50191" "Security Clearance" Opportunity
  , Evidence "50192" "Travel" Opportunity
  , Evidence "50193" "Authority" Opportunity
  ]
