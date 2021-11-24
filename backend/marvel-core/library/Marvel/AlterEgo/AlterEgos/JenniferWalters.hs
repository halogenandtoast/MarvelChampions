module Marvel.AlterEgo.AlterEgos.JenniferWalters where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Attrs
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

jenniferWalters :: AlterEgoCard JenniferWalters
jenniferWalters =
  alterEgo JenniferWalters Cards.jenniferWalters (HP $ Static 15) (HandSize 6) (Rec 5)

newtype JenniferWalters = JenniferWalters AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, HasStartingHP, HasHandSize, ToJSON, FromJSON, IsSource, HasCardCode, Entity)

instance HasAbilities JenniferWalters where
  getAbilities (JenniferWalters a) =
    [ label "\"I Object!\"" $ limited (PerRound 1) $ limitedWindowAbility
        a
        1
        (ThreatWouldBePlaced AnyScheme)
        Interrupt
        IsSelf
        NoCost
        (RunAbility (toTarget a) 1)
    ]

getDetails :: [WindowType] -> (SchemeId, Natural)
getDetails [] = error "wrong window"
getDetails (ThreatPlaced schemeId n : _) = (schemeId, n)
getDetails (_ : xs) = getDetails xs

instance RunMessage JenniferWalters where
  runMessage msg a@(JenniferWalters attrs) = case msg of
    RanAbility target 1 windows | isTarget attrs target -> do
      let
        (schemeId, n) = getDetails windows
        newMsg = case schemeId of
                   SchemeMainSchemeId sid -> MainSchemeMessage sid (MainSchemePlaceThreat (subtractNatural 1 n))
                   SchemeSideSchemeId sid -> SideSchemeMessage sid (SideSchemePlaceThreat (subtractNatural 1 n))
      replaceMatchingMessage [newMsg] $ \case
        (MainSchemeMessage mid (MainSchemePlaceThreat _)) -> schemeId == SchemeMainSchemeId mid
        (SideSchemeMessage mid (SideSchemePlaceThreat _)) -> schemeId == SchemeSideSchemeId mid
        _ -> False

      pure a
    _ -> JenniferWalters <$> runMessage msg attrs
