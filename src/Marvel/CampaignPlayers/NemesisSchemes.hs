module Marvel.CampaignPlayers.NemesisSchemes where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CampaignPlayers.EncounterCards qualified as Cards
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id (CharacterId (MinionCharacter))
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

cn56025, cn56056, cn60034, cn60062 :: SideSchemeCard NemesisScheme
cn56025 = sideScheme NemesisScheme Cards.enc56025 (Static 4)
cn56056 = sideScheme NemesisScheme Cards.enc56056 (Static 5)
cn60034 = sideScheme NemesisScheme Cards.enc60034 (Static 5)
cn60062 = sideScheme NemesisScheme Cards.enc60062 (PerPlayer 2)

newtype NemesisScheme = NemesisScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme NemesisScheme where
  sideSchemeAbilities (NemesisScheme a) = case toCardCode a of
    "56056" -> [windowAbility a 1 (W.CardsDiscardedFromPlayerDeck AnyIdentity) ForcedResponse NoCost $ runAbility a 1]
    "60034" -> [windowAbility a 1 (W.EnemyAttacked W.When (EnemyIs Cards.enc60033) AnyIdentity) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []

instance HasModifiersFor NemesisScheme where
  getModifiersFor _ (MinionRef mid) (NemesisScheme a) = do
    protected <- case toCardCode a of
      "56025" -> member T.Criminal <$> getCharacterTraits (MinionCharacter mid)
      "60062" -> getsGame $ maybe False (sameCard Cards.enc60061) . (`abilityCardDef` MinionRef mid)
      _ -> pure False
    pure [CannotTakeDamage | protected]
  getModifiersFor _ _ _ = pure []

instance RunMessage NemesisScheme where
  runMessage msg e@(NemesisScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a, toCardCode a == "60062" -> do
      you <- getYou
      searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Cards.enc60061)
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> case toCardCode a of
      "56056" -> do
        for_ [cards | W.DiscardedFromPlayerDeck _ cards <- windows] $ traverse_ (placePlayerCardUnder a)
        pure e
      "60034" -> do
        traverse_ dealBoost [enemy | W.EnemyAttack enemy _ <- windows]
        pure e
      _ -> pure e
    CardSpecific (isTarget a -> True) "mgHoldCard" (fromJSON -> Success card) ->
      pure $ NemesisScheme $ a & heldCardsL <>~ [card]
    RemoveFromPlay target | isTarget a target -> do
      traverse_ discardCard (sideSchemeHeldCards a)
      NemesisScheme <$> runMessage msg (a & heldCardsL .~ [])
    RemoveFromGame target | isTarget a target -> do
      traverse_ discardCard (sideSchemeHeldCards a)
      NemesisScheme <$> runMessage msg (a & heldCardsL .~ [])
    AddToVictoryDisplay target | isTarget a target -> do
      traverse_ discardCard (sideSchemeHeldCards a)
      NemesisScheme <$> runMessage msg (a & heldCardsL .~ [])
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "60062" -> do
      kingpins <- selectList $ MinionIs Cards.enc60061
      unless (null kingpins) $ revealBoost a =<< getBoostedPlayer enemy
      pure e
    _ -> NemesisScheme <$> runMessage msg a
