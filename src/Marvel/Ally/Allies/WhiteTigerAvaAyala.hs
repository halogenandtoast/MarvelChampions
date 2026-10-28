module Marvel.Ally.Allies.WhiteTigerAvaAyala (
  whiteTigerAvaAyala,
  WhiteTigerAvaAyala (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source (getVillainStage)
import Marvel.Matchers
import Marvel.Window

whiteTigerAvaAyala :: AllyCard WhiteTigerAvaAyala
whiteTigerAvaAyala = ally WhiteTigerAvaAyala Cards.whiteTigerAvaAyala (Thw 2, 1) (Atk 2, 1) (HP 2)

newtype WhiteTigerAvaAyala = WhiteTigerAvaAyala (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response: After you play White Tiger from your hand, draw X cards (to a maximum of
-- three), X the villain's stage number; with no stage number, 1. Played from hand, not
-- put into play nor played from elsewhere. ponytail: a villain without a stage number
-- has stage 0.
instance HasAbilities WhiteTigerAvaAyala where
  getAbilities a@(WhiteTigerAvaAyala attrs) =
    [ limitedWindowAbility a 1 (PlayedFromHand (IdentityWithId $ allyController attrs) (CardIs Cards.whiteTigerAvaAyala)) Response OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage WhiteTigerAvaAyala where
  runMessage msg a@(WhiteTigerAvaAyala attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      withTheVillain you \villain -> do
        stage <- getVillainStage villain
        drawCards you (if stage == 0 then 1 else min 3 stage)
      pure a
    _ -> WhiteTigerAvaAyala <$> runMessage msg attrs
