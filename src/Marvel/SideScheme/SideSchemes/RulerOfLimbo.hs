module Marvel.SideScheme.SideSchemes.RulerOfLimbo where

import Marvel.Prelude
import Marvel.Support.Types
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Card
import Marvel.Deck
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Projection
import Marvel.Query

rulerOfLimbo :: SideSchemeCard RulerOfLimbo
rulerOfLimbo = sideScheme RulerOfLimbo Cards.rulerOfLimbo (Static 3)

newtype RulerOfLimbo = RulerOfLimbo (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RulerOfLimbo where
  getModifiersFor _ target (RulerOfLimbo attrs) | target == toRef attrs = do
    belasco <- selectAny (MinionIs Minions.belasco)
    pure [ThreatCannotBeRemoved | belasco]
  getModifiersFor _ _ _ = pure []

instance RunMessage RulerOfLimbo where
  runMessage msg a@(RulerOfLimbo attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "holdFacedownCard" (fromJSON -> Success card) ->
      pure $ RulerOfLimbo attrs {sideSchemeHeldCards = sideSchemeHeldCards attrs <> [card]}
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      magik <- selectOne (IdentityWithTitle "Magik" <> HeroIdentity)
      illyana <- selectOne (IdentityWithTitle "Illyana Rasputin" <> AlterEgoIdentity)
      for_ (magik <|> illyana) \you -> do
        inPlay <- selectList (SupportNamed "Limbo")
        case inPlay of
          support : _ -> do
            game <- getGame
            def <- maybe (error "Limbo has no card definition") pure (abilityCardDef game $ SupportRef support)
            let card = MkPlayerCard (CardId $ unSupportId support) def (Just you) (Just you)
            removeFromGame support
            holdFacedownCard attrs card
          [] -> do
            deck <- projectMap PlayerIdentityDeck unDeck you
            found <- findOwnedPlayerCard you "45032"
            for_ found \card -> do
              holdFacedownCard attrs card
              when (card `elem` deck) $ shuffleDeck you
      pure a
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      let limboCards = filter ((== "45032") . toCardCode) (sideSchemeHeldCards attrs)
      for_ limboCards \card -> for_ (pcOwner card) (\owner -> putIntoPlay owner card)
      RulerOfLimbo <$> runMessage msg attrs {sideSchemeHeldCards = filter ((/= "45032") . toCardCode) (sideSchemeHeldCards attrs)}
    _ -> RulerOfLimbo <$> runMessage msg attrs
