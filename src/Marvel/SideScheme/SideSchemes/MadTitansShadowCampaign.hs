-- | The side schemes of The Mad Titan's Shadow's campaign set: one put into play by each
-- scenario's Campaign Instructions, its A side flipping into its B side as it is defeated,
-- and Security Breach, shuffled into the encounter deck.
module Marvel.SideScheme.SideSchemes.MadTitansShadowCampaign (
  secureTheLandingPad,
  SecureTheLandingPad (..),
  securityBreach,
  SecurityBreach (..),
  saveTheShawarmaPlace,
  SaveTheShawarmaPlace (..),
  hackSanctuarysComputer,
  HackSanctuarysComputer (..),
  defensiveProtocols,
  DefensiveProtocols (..),
  findTheNornStones,
  FindTheNornStones (..),
  retrieveOdinsArmor,
  RetrieveOdinsArmor (..),
  openTheDungeons,
  OpenTheDungeons (..),
) where

import Marvel.Prelude

import Marvel.Ally.Cards qualified as Allies
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card
import Marvel.Count
import Marvel.EncounterCard (genEncounterCard)
import Marvel.EncounterSet (EncounterSet (MadTitansShadowCampaign))
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id (IdentityId)
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Name (title)
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Phase (Phase (PlayerPhase))
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Resource.Cards qualified as Resources
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait (Trait (Captive, Wounded))
import Marvel.Upgrade.Cards qualified as Upgrades
import System.Random.Shuffle (shuffleM)

-- | A new copy of a campaign-set player card, the player's own.
newCard :: (MonadRandom m) => IdentityId -> CardDef -> m PlayerCard
newCard ident def = do
  cid <- getRandom
  pure $ MkPlayerCard cid def (Just ident) (Just ident)

-- | "Flip this card over": a double-sided card, its other side of another type takes its
-- place (RR Flip, Double-Sided Card), the side scheme removed from the game; the other
-- side enters play as that card would be put into play, for the first player (a side
-- scheme with its starting threat and hinder, a minion engaged with them, an attachment
-- attached as it says).
flipInto :: (ReverseQueue m) => Attrs SideScheme -> CardDef -> m ()
flipInto attrs def = do
  first <- selectJust FirstPlayer
  removeFromGame attrs
  putEncounterCardIntoPlay first =<< genEncounterCard def

secureTheLandingPad :: SideSchemeCard SecureTheLandingPad
secureTheLandingPad = sideScheme SecureTheLandingPad Cards.secureTheLandingPad (Static 2)

newtype SecureTheLandingPad = SecureTheLandingPad (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Hinder 1 per player (the def). When Defeated: flip this card over: Cosmo, under the
-- first player's control.
instance RunMessage SecureTheLandingPad where
  runMessage msg s@(SecureTheLandingPad attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      first <- selectJust FirstPlayer
      removeFromGame attrs
      putIntoPlay first =<< newCard first Allies.cosmoCampaign
      pure s
    _ -> SecureTheLandingPad <$> runMessage msg attrs

securityBreach :: SideSchemeCard SecurityBreach
securityBreach = sideScheme SecurityBreach Cards.securityBreach (Static 2)

newtype SecurityBreach = SecurityBreach (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Hinder 2 per player, Victory 2 (the def). When Revealed: each player places a
-- random card from their hand facedown here. When Defeated: return each facedown card
-- here to its owner's hand.
instance RunMessage SecurityBreach where
  runMessage msg (SecurityBreach attrs) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        players <- getPlayers
        taken <- concat <$> for players \p -> take 1 <$> (shuffleM =<< projectMap PlayerIdentityHand unHand p)
        for_ taken (removeFromGame . CardIdRef . pcCardId)
        cardSpecific attrs "holdTakenCards" taken
        pure $ SecurityBreach attrs
      DefeatSideScheme -> do
        for_ [(owner, c) | c <- sideSchemeHeldCards attrs, Just owner <- [pcOwner c]] (uncurry addToHand)
        pure . SecurityBreach $ attrs & heldCardsL .~ []
      _ -> SecurityBreach <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "holdTakenCards" (fromJSON -> Success cards) ->
      pure . SecurityBreach $ attrs & heldCardsL <>~ cards
    _ -> SecurityBreach <$> runMessage msg attrs

saveTheShawarmaPlace :: SideSchemeCard SaveTheShawarmaPlace
saveTheShawarmaPlace = sideScheme SaveTheShawarmaPlace Cards.saveTheShawarmaPlace (Static 2)

newtype SaveTheShawarmaPlace = SaveTheShawarmaPlace (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Hinder 1 per player (the def). When Defeated: each player shuffles 1 copy of
-- Shawarma into their deck. Flip this card over: Black Swan, engaged with the first
-- player.
instance RunMessage SaveTheShawarmaPlace where
  runMessage msg s@(SaveTheShawarmaPlace attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p -> shuffleIntoDeck p . pure =<< newCard p Resources.shawarma
      flipInto attrs Minions.blackSwan
      pure s
    _ -> SaveTheShawarmaPlace <$> runMessage msg attrs

hackSanctuarysComputer :: SideSchemeCard HackSanctuarysComputer
hackSanctuarysComputer = sideScheme HackSanctuarysComputer Cards.hackSanctuarysComputer (Static 2)

newtype HackSanctuarysComputer = HackSanctuarysComputer (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Hinder 1 per player (the def). When Defeated: each player searches their deck and
-- discard pile for 1 card, adds it to their hand, and shuffles their deck. Flip this card
-- over: Defensive Protocols.
instance RunMessage HackSanctuarysComputer where
  runMessage msg s@(HackSanctuarysComputer attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p -> search p (SearchIdentityDeckAndDiscardPile p) AnyCard SearchDrawOne ShuffleBackIn
      flipInto attrs Cards.defensiveProtocols
      pure s
    _ -> HackSanctuarysComputer <$> runMessage msg attrs

defensiveProtocols :: SideSchemeCard DefensiveProtocols
defensiveProtocols = sideScheme DefensiveProtocols Cards.defensiveProtocols (Static 2)

newtype DefensiveProtocols = DefensiveProtocols (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Hinder 2 per player, Victory 2 (the def). Forced Interrupt: when the player phase
-- ends, place 1 crash counter here. If there are 2 crash counters here, each player adds
-- 1 copy of the System Shock obligation card to their hand. Remove this card from the
-- game.
instance RunMessage DefensiveProtocols where
  runMessage msg s@(DefensiveProtocols attrs) = runQueueT $ case msg of
    EndPhase PlayerPhase -> do
      placeCounters attrs 1
      when (sideSchemeCounters attrs + 1 >= 2) do
        players <- getPlayers
        for_ players \p -> addToHand p =<< newCard p Obligations.systemShock
        removeFromGame attrs
      pure s
    _ -> DefensiveProtocols <$> runMessage msg attrs

findTheNornStones :: SideSchemeCard FindTheNornStones
findTheNornStones = sideScheme FindTheNornStones Cards.findTheNornStones (Static 3)

newtype FindTheNornStones = FindTheNornStones (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Threat cannot be removed from this scheme unless Hela has the Wounded trait.
instance HasModifiersFor FindTheNornStones where
  getModifiersFor _ (SideSchemeRef sid) (FindTheNornStones a)
    | sid == sideSchemeId a = do
        wounded <- selectAny (VillainWithTrait Wounded)
        pure [ThreatCannotBeRemoved | not wounded]
  getModifiersFor _ _ _ = pure []

-- | When Defeated: each player puts a copy of the Norn Stone upgrade into play under
-- their control on its Setup side. Flip this card over: Retrieve Odin's Armor.
instance RunMessage FindTheNornStones where
  runMessage msg s@(FindTheNornStones attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p -> putIntoPlay p =<< newCard p Upgrades.nornStoneSetup
      flipInto attrs Cards.retrieveOdinsArmor
      pure s
    _ -> FindTheNornStones <$> runMessage msg attrs

retrieveOdinsArmor :: SideSchemeCard RetrieveOdinsArmor
retrieveOdinsArmor = sideScheme RetrieveOdinsArmor Cards.retrieveOdinsArmor (Static 2)

newtype RetrieveOdinsArmor = RetrieveOdinsArmor (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Odin, on either side.
odinAlly :: AllyMatcher
odinAlly = AllyWithTitle "Odin"

-- | Hinder 2 per player, Victory 1 (the def). Threat cannot be removed from this scheme
-- unless the first player controls Odin.
instance HasModifiersFor RetrieveOdinsArmor where
  getModifiersFor _ (SideSchemeRef sid) (RetrieveOdinsArmor a)
    | sid == sideSchemeId a = do
        controlled <- selectAny (odinAlly <> AllyControlledBy FirstPlayer)
        pure [ThreatCannotBeRemoved | not controlled]
  getModifiersFor _ _ _ = pure []

-- | When Defeated: heal all damage from Odin and flip him to his King side.
instance RunMessage RetrieveOdinsArmor where
  runMessage msg s@(RetrieveOdinsArmor attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      selectOne (AllyIs Allies.odinCaptive) >>= traverse_ \odin -> do
        damage <- selectCount AllyDamageValue (AllyWithId odin)
        when (damage > 0) $ heal odin damage
        flipAlly odin
      pure s
    _ -> RetrieveOdinsArmor <$> runMessage msg attrs

openTheDungeons :: SideSchemeCard OpenTheDungeons
openTheDungeons = sideScheme OpenTheDungeons Cards.openTheDungeons (Static 3)

newtype OpenTheDungeons = OpenTheDungeons (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: each player chooses 1 Captive ally from the campaign set and puts it
-- into play under their control, in player order, each from those still set aside (one
-- whose title a unique card of theirs in play has is not theirs to choose). Flip this
-- card over: Jormungand, attached to Loki.
instance RunMessage OpenTheDungeons where
  runMessage msg s@(OpenTheDungeons attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players (cardSpecific attrs "freeCaptive")
      flipInto attrs Attachments.jormungand
      pure s
    CardSpecific (isTarget attrs -> True) "freeCaptive" (fromJSON -> Success p) -> do
      setAside' <- getSetAsidePlayerCards
      taken <- asYou p $ map fst <$> getUniqueTitlesInPlay
      let captives =
            [ c
            | c <- setAside'
            , let def = getCardDef c
            , Captive `member` cdTraits def
            , cdEncounterSet def == Just MadTitansShadowCampaign
            , title (cdName def) `notElem` taken
            ]
      chooseOnePrompted p "$label.chooseCaptiveAlly"
        [ cardLabeled (toCardCode c) do
            removeFromGame (CardIdRef $ pcCardId c)
            putIntoPlay p c {pcOwner = Just p, pcController = Just p}
        | c <- captives
        ]
      pure s
    _ -> OpenTheDungeons <$> runMessage msg attrs
