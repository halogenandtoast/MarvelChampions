module Marvel.MainScheme.MainSchemes.HuntingDownHeroes where

import Marvel.Prelude

import Marvel.Ally.Cards qualified as Allies
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.SideScheme.Cards (allSideSchemes)

huntingDownHeroes :: MainSchemeCard HuntingDownHeroes
huntingDownHeroes =
  mainScheme HuntingDownHeroes Cards.huntingDownHeroes (PerPlayer 12) (PerPlayer 1) (PerPlayer 1)

newtype HuntingDownHeroes = HuntingDownHeroes (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | The Captive allies of the Taskmaster set, the cards set aside out of play.
captiveAllies :: [CardDef]
captiveAllies = [Allies.moonKnightMarcSpector, Allies.shangChi, Allies.whiteTigerAngelaDelToro, Allies.elektra]

instance RunMessage HuntingDownHeroes where
  runMessage msg ms@(HuntingDownHeroes attrs) = runQueueT $ case msg of
    -- 1A Setup: "Set each Captive ally aside out of play. Search the encounter deck for
    -- Hydra Patrol and put it into play. Shuffle the encounter deck."
    SetupMainScheme -> do
      captives <- for captiveAllies \def -> do
        cardId <- getRandom
        pure $ PlayerCard $ MkPlayerCard cardId def Nothing Nothing
      setAside captives
      you <- getYou
      -- Hydra Patrol is the Hydra Patrol set's (04154)
      for_ (lookup "04154" allSideSchemes) \hydraPatrol ->
        searchAndPutIntoPlayEncounterCard you (CardIs hydraPatrol)
      pure ms
    -- 1B: "Forced Response: After resolving step one of the villain phase, each player in
    -- hero form must choose to either place 1 threat here or take 1 damage."
    AccelerateMainScheme -> do
      ms' <- HuntingDownHeroes <$> runMessage msg attrs
      heroes <- filterM (identityMatches HeroIdentity) =<< getPlayers
      for_ heroes \p ->
        chooseOne
          p
          [ labeled "$label.placeOneThreatOnMainScheme" $ placeThreat (mainSchemeId attrs) 1
          , labeled "$label.take1Damage" $ damageIdentity p attrs (toDamage 1 FromAbility)
          ]
      pure ms'
    AdvanceMainScheme -> ms <$ gameOver Lost
    _ -> HuntingDownHeroes <$> runMessage msg attrs
