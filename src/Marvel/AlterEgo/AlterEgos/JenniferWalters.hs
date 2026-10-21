module Marvel.AlterEgo.AlterEgos.JenniferWalters where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Stats
import Marvel.Window

jenniferWalters :: AlterEgoCard JenniferWalters
jenniferWalters =
  alterEgo
    JenniferWalters
    Cards.jenniferWalters
    (HP $ Static 15)
    (HandSize 6)
    (Rec 5)
    [Cards.legalWork]

newtype JenniferWalters = JenniferWalters (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities JenniferWalters where
  getAbilities a =
    [ label "\"I Object!\""
        $ limited (PerRound 1)
        $ limitedWindowAbility
          a
          1
          (ThreatWouldBePlaced AnyThreatSource AnyScheme)
          Interrupt
          IsSelf
          NoCost
        $ runAbility a 1
    ]

getDetails :: [WindowType] -> (SchemeId, Natural)
getDetails [] = error "wrong window"
getDetails (ThreatPlaced _ schemeId n : _) = (schemeId, n)
getDetails (_ : xs) = getDetails xs

instance RunMessage JenniferWalters where
  runMessage msg a@(JenniferWalters attrs) = case msg of
    RanAbility _ (isTarget attrs -> True) 1 (getDetails -> (schemeId, n)) _ -> do
      let
        newMsg = case schemeId of
          SchemeMainSchemeId sid ->
            MainSchemeMessage sid (MainSchemePlaceThreat (subtractNatural 1 n))
          SchemeSideSchemeId sid ->
            SideSchemeMessage sid (SideSchemePlaceThreat (subtractNatural 1 n))
      replaceMatchingMessage (const [newMsg]) $ \case
        (MainSchemeMessage mid (MainSchemePlaceThreat _)) ->
          schemeId == SchemeMainSchemeId mid
        (SideSchemeMessage mid (SideSchemePlaceThreat _)) ->
          schemeId == SchemeSideSchemeId mid
        _ -> False
      pure a
    _ -> JenniferWalters <$> runMessage msg attrs
