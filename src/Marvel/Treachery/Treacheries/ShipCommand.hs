-- | The Ship Command modular set's treacheries (The Galaxy's Most Wanted). Blind Side,
-- Hull Breach and Power Siphon have peril: no one else may help the player resolving
-- them. Each lets that player "exhaust the Milano" whether or not they control it (TGMW
-- rulebook FAQ, p.21).
module Marvel.Treachery.Treacheries.ShipCommand (
  blindSide,
  BlindSide (..),
  hullBreach,
  HullBreach (..),
  powerSiphon,
  PowerSiphon (..),
  specialDelivery,
  SpecialDelivery (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Id (IdentityId)
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

-- | "Choose one: exhaust the Milano; spend these resources; or what befalls the first
-- player." An encounter card's option with no valid target cannot be chosen (RR Choose):
-- the Milano must be in play and ready, the resources there to spend, and the first
-- player open to what the last option does.
chooseMilanoOr :: (ReverseQueue m) => IdentityId -> Text -> Resource -> (IdentityId -> m Bool) -> (IdentityId -> m ()) -> Text -> m ()
chooseMilanoOr you spendLabel resource canBefall befall befallLabel = do
  ready <- milanoIsReady
  let cost = MultiResourceCost [Just resource, Just resource]
  payable <- canAffordCost you (toSource you) cost
  first <- selectJust FirstPlayer
  befalls <- canBefall first
  chooseOrRunOne you $
    [labeled "$label.exhaustTheMilano" exhaustMilano | ready]
      <> [labeled (countLabel spendLabel 2) (payForTreachery you cost) | payable]
      <> [labeled befallLabel (befall first) | befalls]

blindSide :: TreacheryCard BlindSide
blindSide = treachery BlindSide Cards.blindSide

-- | When Revealed: exhaust the Milano, spend [physical][physical], or stun the first
-- player (one already stunned is no target for it).
newtype BlindSide = BlindSide (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BlindSide where
  runMessage msg t@(BlindSide attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      chooseMilanoOr you "spendPhysicalResources" Physical (fmap not . identityMatches StunnedIdentity) stunIdentity "$label.stunTheFirstPlayer"
      pure t
    _ -> BlindSide <$> runMessage msg attrs

hullBreach :: TreacheryCard HullBreach
hullBreach = treachery HullBreach Cards.hullBreach

-- | When Revealed: exhaust the Milano, spend [mental][mental], or deal 3 damage to the
-- first player.
newtype HullBreach = HullBreach (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HullBreach where
  runMessage msg t@(HullBreach attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      chooseMilanoOr you "spendMentalResources" Mental (const $ pure True) (\first -> damageIdentity first attrs (toDamage 3 FromTreachery)) (countLabel "dealDamageToTheFirstPlayer" 3)
      pure t
    _ -> HullBreach <$> runMessage msg attrs

powerSiphon :: TreacheryCard PowerSiphon
powerSiphon = treachery PowerSiphon Cards.powerSiphon

-- | When Revealed: exhaust the Milano, spend [energy][energy], or discard 1 card at random
-- from the first player's hand (one with an empty hand is no target for it).
newtype PowerSiphon = PowerSiphon (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PowerSiphon where
  runMessage msg t@(PowerSiphon attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      let hasCards = projectP PlayerIdentityHand (notNull . unHand)
      chooseMilanoOr you "spendEnergyResources" Energy hasCards (\first -> discardFrom first RandomFromHand 1) "$label.discardRandomCardFromFirstPlayersHand"
      pure t
    _ -> PowerSiphon <$> runMessage msg attrs

specialDelivery :: TreacheryCard SpecialDelivery
specialDelivery = treachery SpecialDelivery Cards.specialDelivery

-- | When Revealed (Alter-Ego): you may exhaust the Milano; if you do not, the villain
-- schemes with +1 SCH. (Hero): you may exhaust the Milano; if you do not, the villain
-- attacks you with +1 ATK.
newtype SpecialDelivery = SpecialDelivery (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SpecialDelivery where
  runMessage msg t@(SpecialDelivery attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      ready <- milanoIsReady
      hero <- identityMatches HeroIdentity you
      let activates = selectOne ActiveVillain >>= traverse_ \villain -> do
            activationBonus villain 1
            if hero then enemyAttacks villain you else villainSchemes villain
      if ready
        then
          chooseOne you
            [ labeled "$label.exhaustTheMilano" exhaustMilano
            , labeled "$label.doNotExhaustTheMilano" activates
            ]
        else activates
      pure t
    _ -> SpecialDelivery <$> runMessage msg attrs
