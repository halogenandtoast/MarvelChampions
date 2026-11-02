module Marvel.MainScheme.MainSchemes.TricksterTakeover where

import System.Random.Shuffle (shuffleM)
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.TricksterTakeover.Helpers
import Marvel.Window qualified as W
import Marvel.MainScheme.Types

newtype TTMainScheme = TTMainScheme (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMainScheme TTMainScheme where
  mainSchemeAbilities = getAbilities

tt55004a :: MainSchemeCard TTMainScheme
tt55004a = mainScheme TTMainScheme Cards.tt55004a (PerPlayer 0) (PerPlayer 0) (PerPlayer 0)

tt55004b :: MainSchemeCard TTMainScheme
tt55004b = mainScheme TTMainScheme Cards.tt55004b (PerPlayer 6) (PerPlayer 1) (PerPlayer 1)

tt55005a :: MainSchemeCard TTMainScheme
tt55005a = mainScheme TTMainScheme Cards.tt55005a (PerPlayer 0) (PerPlayer 0) (PerPlayer 0)

tt55005b :: MainSchemeCard TTMainScheme
tt55005b = mainScheme TTMainScheme Cards.tt55005b (PerPlayer 9) (PerPlayer 2) (PerPlayer 1)

tt55028a :: MainSchemeCard TTMainScheme
tt55028a = mainScheme TTMainScheme Cards.tt55028a (PerPlayer 0) (PerPlayer 0) (PerPlayer 0)

tt55028b :: MainSchemeCard TTMainScheme
tt55028b = mainScheme TTMainScheme Cards.tt55028b (PerPlayer 2) (PerPlayer 0) (PerPlayer 0)

tt55033a :: MainSchemeCard TTMainScheme
tt55033a = mainScheme TTMainScheme Cards.tt55033a (PerPlayer 0) (PerPlayer 0) (PerPlayer 0)

tt55033b :: MainSchemeCard TTMainScheme
tt55033b = mainScheme TTMainScheme Cards.tt55033b (PerPlayer 8) (PerPlayer 0) (PerPlayer 1)

instance HasAbilities TTMainScheme where
  getAbilities (TTMainScheme a) =
    [windowAbility a 1 (W.IdentityWouldBeDefeated AnyIdentity) ForcedInterrupt NoCost (runAbility a 1) | toCardCode a == "55033b"]

instance RunMessage TTMainScheme where
  runMessage msg e@(TTMainScheme a) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      case toCardCode a of
        "55004b" -> do
          cards <- shuffleM [Cards.tt55007a, Cards.tt55008a, Cards.tt55009a, Cards.tt55010a, Cards.tt55011a]
          players <- getAllPlayers
          for_ (zip players cards) $ \(p,def) -> putEncounterCardIntoPlay p =<< genEncounterCard def
          setAside . map EncounterCard =<< traverse genEncounterCard (Cards.tt55006 : drop (length players) cards)
        "55033b" -> do
          for_ [Cards.tt55052, Cards.tt55053, Cards.tt55054, Cards.tt55055] $ \def -> putEncounterCardIntoPlay first =<< genEncounterCard def
          hard <- expert
          if hard then putEncounterCardIntoPlay first =<< genEncounterCard Cards.tt55034a
            else setAside . map EncounterCard =<< traverse genEncounterCard [Cards.tt55034a]
        _ -> pure ()
      pure e
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a, toCardCode a == "55005b" -> do
      crown <- selectAny (AttachmentIs Cards.tt55016)
      if crown then getAllPlayers >>= traverse_ stunIdentity
        else selectJust FirstPlayer >>= \p -> searchAndRevealEncounterCard p (CardIs Cards.tt55016)
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a, toCardCode a == "55033b" -> do
      cardSpecific GameRef "ttWorldsThreat" ()
      pure $ TTMainScheme a {mainSchemeThreat = 0}
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      let players = [p | W.IdentityDefeat p <- windows]
      for_ (if null players then [you] else players) $ \p -> do
        insteadOf (== IdentityMessage p IdentityDefeated) $ do
          changeToAlterEgo p
          setHitPoints p 1
          cardSpecific GameRef "ttWorldsThreat" ()
      pure e
    _ -> TTMainScheme <$> runMessage msg a
