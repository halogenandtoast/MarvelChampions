module Marvel.SideScheme.SideSchemes.FamilyFeud (
  familyFeud,
  FamilyFeud (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

familyFeud :: SideSchemeCard FamilyFeud
familyFeud = sideScheme FamilyFeud Cards.familyFeud (Static 2)

newtype FamilyFeud = FamilyFeud (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: 1 more threat for each Asgard card in play. ponytail: identities,
-- allies, supports, upgrades and minions, the cards with traits that come into play so
-- far
instance RunMessage FamilyFeud where
  runMessage msg s@(FamilyFeud attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        n <-
          sum
            <$> sequence
              [ selectListCount (IdentityWithTrait Asgard)
              , selectListCount (AllyWithTrait Asgard)
              , selectListCount (SupportWithTrait Asgard)
              , selectListCount (UpgradeWithTrait Asgard)
              , selectListCount (MinionWithTrait Asgard)
              ]
        pushAll $ placeThreatOn (SchemeSideSchemeId $ sideSchemeId attrs) n
        pure s
      _ -> FamilyFeud <$> runMessage msg attrs
    _ -> FamilyFeud <$> runMessage msg attrs
