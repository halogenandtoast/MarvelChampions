module Marvel.SideScheme.SideSchemes.SuperpowerTraining where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

superpowerTraining :: SideSchemeCard SuperpowerTraining
superpowerTraining = sideScheme SuperpowerTraining Cards.superpowerTraining (PerPlayer 3)
newtype SuperpowerTraining = SuperpowerTraining (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SuperpowerTraining where
  runMessage msg a@(SuperpowerTraining attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- selectList AnyIdentity
      for_ players $ cardSpecific attrs "search"
      pure a
    CardSpecific (isTarget attrs -> True) "search" (fromJSON -> Success you) -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      allowed <- selectList (BasicCardMatches (HeroSpecificCard <> CardWithType UpgradeType) <> CanEnterPlay)
      let candidates = filter (`elem` allowed) (deck <> discard)
      cards <- filterM (fmap (maybe True notNull) . getPlayTargets you) candidates
      focusing (PlayerCard <$> cards) $
        chooseOne you $ labeled "$label.noResponse" (pure ()) : [targeting card (cardSpecific attrs "putUpgrade" (you,card)) | card <- cards]
      shuffleDeck you
      pure a
    CardSpecific (isTarget attrs -> True) "putUpgrade" (fromJSON -> Success (you,card)) -> do
      targets <- getPlayTargets you card
      case targets of
        Nothing -> putIntoPlay you card
        Just ts -> chooseOrRunOne you [targeting target (putIntoPlayOn you card target) | target <- ts]
      pure a
    _ -> SuperpowerTraining <$> runMessage msg attrs
