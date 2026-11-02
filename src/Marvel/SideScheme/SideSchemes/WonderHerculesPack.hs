module Marvel.SideScheme.SideSchemes.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.WonderMan.Cards qualified as WM
import Marvel.Hercules.Cards qualified as HC
import Marvel.GameValue

wm58027 :: SideSchemeCard WmHcSideScheme
wm58027 = sideScheme WmHcSideScheme WM.wm58027 (PerPlayer 2)

hc59028 :: SideSchemeCard WmHcSideScheme
hc59028 = sideScheme WmHcSideScheme HC.hc59028 (PerPlayer 2)

hc59038 :: SideSchemeCard WmHcSideScheme
hc59038 = sideScheme WmHcSideScheme HC.hc59038 (Static 2)

hc59041 :: SideSchemeCard WmHcSideScheme
hc59041 = sideScheme WmHcSideScheme HC.hc59041 (Static 2)

hc59045 :: SideSchemeCard WmHcSideScheme
hc59045 = sideSchemeWith WmHcSideScheme HC.hc59045 (Static 3) (\a -> a {sideSchemeCrisis = True})

newtype WmHcSideScheme = WmHcSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme WmHcSideScheme where
  sideSchemeAbilities = getAbilities

instance HasModifiersFor WmHcSideScheme where
  getModifiersFor _ target (WmHcSideScheme attrs) | toCardCode attrs == "58027" = do
    reapers <- selectList $ MinionIs WM.wm58026
    pure [EnemyAttacksAdditionalCost $ DiscardHandCardsCost 1 $ Just 1 | target `elem` map MinionRef reapers]
  getModifiersFor _ _ _ = pure []

instance HasAbilities WmHcSideScheme where
  getAbilities s@(WmHcSideScheme attrs) = case toCardCode attrs of
    "59041" -> [limitedWindowAbility s 1 (W.CharacterDefeated W.After AnyCharacter) ForcedResponse NoCriteria NoCost $ runAbility s 1]
    _ -> []

instance RunMessage WmHcSideScheme where
  runMessage msg s@(WmHcSideScheme attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "59041" -> s <$ placeThreatOn attrs (sideSchemeId attrs) 2
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs && toCardCode attrs == "59038" -> do
      n <- countCardsInPlayWithTrait T.Olympus
      placeThreatOn attrs sid n
      pure s
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      case toCardCode attrs of
        "59028" -> do
          players <- getPlayers
          for_ players $ \you -> chooseOne you
            [labeled "$label.done" (pure ()), labeled "$label.searchDeck" $ search you (SearchIdentityDeckAndDiscardPile you) (CardMatchesAny [CardWithTrait T.Avenger, CardWithTrait T.Shield]) (SearchTarget $ toRef attrs) ShuffleBackIn]
        "59041" -> setAside [toCard attrs]
        "59045" -> do
          you <- getYou
          searchEverywhereAndRevealEncounterCard you $ CardIs HC.hc59041
        _ -> pure ()
      pure s
    SearchFoundCards (isTarget attrs -> True) (onlyPlayerCards -> cards) | toCardCode attrs == "59028" -> do
      for_ cards $ \card -> for_ (pcOwner card) $ \you -> do
        addToHand you card
        lastingModifiers attrs (IdentityEntity $ IdentityWithId you) [ResourceCostReductionForCard (pcCardId card) 2] False
      pure s
    _ -> WmHcSideScheme <$> runMessage msg attrs
