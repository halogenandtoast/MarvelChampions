-- | Infiltrate the Museum's treacheries (The Galaxy's Most Wanted); its Caught Off Guard is
-- the core set's ('Marvel.Treachery.Treacheries.CaughtOffGuard').
module Marvel.Treachery.Treacheries.InfiltrateTheMuseum (
  inconspicuousBox,
  InconspicuousBox (..),
  viewTheCosmos,
  ViewTheCosmos (..),
  stayAwhile,
  StayAwhile (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Cost
import Marvel.Deck
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityAttackDamageTaken, PlayerIdentityDeck, PlayerIdentityHand))
import Marvel.Keyword (Keyword (Permanent))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Resource.Types (Resource (Physical))
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

printedCost :: (HasCardDef a) => a -> Natural
printedCost = maybe 0 (fromIntegral . max 0) . cdCost . getCardDef

inconspicuousBox :: TreacheryCard InconspicuousBox
inconspicuousBox = treachery InconspicuousBox Cards.inconspicuousBox

-- | When Revealed: put the lowest cost card you control (an ally, support or upgrade; a
-- permanent one cannot leave play) faceup into The Collection; if you cannot, this card
-- gains surge. [star] Boost: if there are 3 per player or fewer cards in The Collection,
-- put the top card of your deck faceup into The Collection.
newtype InconspicuousBox = InconspicuousBox (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage InconspicuousBox where
  runMessage msg t@(InconspicuousBox attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
      supports <- selectList (SupportControlledBy $ IdentityWithId you)
      g <- getGame
      let costed =
            [ (t', printedCost def)
            | t' <- map AllyRef allies <> map UpgradeRef upgrades <> map SupportRef supports
            , Just def <- [abilityCardDef g t']
            , Permanent `notElem` cdKeywords def
            ]
      case extremes LeastThreat costed of
        [] -> pure . InconspicuousBox $ attrs & surgeL .~ True
        lowest -> do
          chooseOrRunOne you [targeting c (placeInCollectionFromPlay c) | c <- lowest]
          pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      n <- length <$> getCollection
      limit <- fromGameValue (PerPlayer 3)
      when (n <= limit) $ placeTopOfDeckInCollection you
      pure t
    _ -> InconspicuousBox <$> runMessage msg attrs

viewTheCosmos :: TreacheryCard ViewTheCosmos
viewTheCosmos = treachery ViewTheCosmos Cards.viewTheCosmos

-- | When Revealed: choose one: put the highest cost card from your hand faceup into The
-- Collection; or discard the highest cost card from your hand, then place threat on the
-- main scheme equal to its printed cost. Tied cards are the player's to pick; with no
-- card in hand neither can be resolved.
newtype ViewTheCosmos = ViewTheCosmos (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ViewTheCosmos where
  runMessage msg t@(ViewTheCosmos attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      hand <- projectMap PlayerIdentityHand unHand you
      let highest = extremes MostThreat [(c, printedCost c) | c <- hand]
      when (notNull highest) do
        chooseOne you
          [ labeled "$label.highestCostIntoTheCollection" $
              chooseOrRunOne you [targeting c (placeInCollection c) | c <- highest]
          , labeled "$label.discardHighestCostForThreat" $
              chooseOrRunOne you
                [ targeting c do
                    discardCard c
                    selectOne MainScheme >>= traverse_ (`placeThreat` printedCost c)
                | c <- highest
                ]
          ]
      pure t
    _ -> ViewTheCosmos <$> runMessage msg attrs

stayAwhile :: TreacheryCard StayAwhile
stayAwhile = treachery StayAwhile Cards.stayAwhile

-- | When Revealed (Alter-Ego): choose to either spend [physical][physical] resources or put
-- the top card of your deck faceup into The Collection. (Hero): Collector attacks you with
-- +1 ATK; if you take any amount of damage from that attack, put the top card of your
-- deck faceup into The Collection.
newtype StayAwhile = StayAwhile (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage StayAwhile where
  runMessage msg t@(StayAwhile attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      if hero
        then selectOne ActiveVillain >>= traverse_ \villain -> do
          activationBonus villain 1
          enemyAttacks villain you
          cardSpecific attrs "afterTheAttack" you
        else do
          let cost = MultiResourceCost [Just Physical, Just Physical]
          payable <- canAffordCost you (toSource you) cost
          cards <- projectP PlayerIdentityDeck (notNull . unDeck) you
          chooseOrRunOne you $
            [labeled (countLabel "spendPhysicalResources" 2) (payForTreachery you cost) | payable]
              <> [labeled "$label.topCardIntoTheCollection" (placeTopOfDeckInCollection you) | cards]
      pure t
    -- the damage the attack dealt the identity: none if it was defended by an ally, or
    -- prevented
    CardSpecific (isTarget attrs -> True) "afterTheAttack" (fromJSON -> Success you) -> do
      -- a player the attack defeated has no deck left to take from
      inGame <- elem you <$> getAllPlayers
      when inGame do
        taken <- project PlayerIdentityAttackDamageTaken you
        when (taken > 0) $ placeTopOfDeckInCollection you
      pure t
    _ -> StayAwhile <$> runMessage msg attrs
