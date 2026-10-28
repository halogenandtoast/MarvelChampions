-- | The side schemes of Hela's set, and of the Legions of Hel and Frost Giants modular
-- sets (The Mad Titan's Shadow, scenario 4). Hela's three are the road through Hel:
-- Gnipahellir is revealed at setup, and each When Defeated reveals the next, set aside,
-- with the minion that guards it.
module Marvel.SideScheme.SideSchemes.Hela (
  gnipahellir,
  Gnipahellir (..),
  gjallerbru,
  Gjallerbru (..),
  hallOfNastrond,
  HallOfNastrond (..),
  legionsOfHelScheme,
  LegionsOfHelScheme (..),
  unnaturalStorm,
  UnnaturalStorm (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait (Trait (Undead))

-- | The first player reveals the set-aside cards, in order.
revealSetAside :: (ReverseQueue m) => [CardDef] -> m ()
revealSetAside defs = do
  first <- selectJust FirstPlayer
  setAsideCards <- getSetAsideEncounterCards
  for_ defs \def -> for_ (take 1 [c | c <- setAsideCards, getCardDef c == def]) (revealEncounterCard first)

-- | "Deal each other player 1 facedown encounter card": each but the first player.
dealEachOtherPlayer :: (ReverseQueue m) => m ()
dealEachOtherPlayer = do
  first <- selectJust FirstPlayer
  traverse_ dealEncounterCard . filter (/= first) =<< getPlayers

-- | The cards matching in the side scheme's own game area (nothing in one area affects
-- another).
inAreaOf :: (HasGame m, IsRef a) => Attrs SideScheme -> [a] -> m [a]
inAreaOf a xs = do
  here <- getAreaOfTarget (SideSchemeRef $ sideSchemeId a)
  filterM (fmap (== here) . getAreaOfTarget) xs

-- | "Threat cannot be removed from <this scheme>" while its guardian is in play, in its
-- area.
guardedBy :: (HasGame m) => CardDef -> Attrs SideScheme -> Target -> m [Modifier]
guardedBy guardian a = \case
  SideSchemeRef sid | sid == sideSchemeId a -> do
    guards <- inAreaOf a . map MinionRef =<< inAnyArea (selectList (MinionIs guardian))
    pure [ThreatCannotBeRemoved | notNull guards]
  _ -> pure []

-- | 2 threat. Hinder 1[per hero], victory 2, an acceleration icon (its card's).
gnipahellir :: SideSchemeCard Gnipahellir
gnipahellir = sideScheme Gnipahellir Cards.gnipahellir (Static 2)

newtype Gnipahellir = Gnipahellir (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Garm: "Threat cannot be removed from Gnipahellir."
instance HasModifiersFor Gnipahellir where
  getModifiersFor _ target (Gnipahellir a) = guardedBy Minions.garm a target

-- | When Defeated: the first player reveals Gjallerbru and Skurge, and puts them into
-- play. Deal each other player 1 facedown encounter card.
instance RunMessage Gnipahellir where
  runMessage msg (Gnipahellir attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      revealSetAside [Cards.gjallerbru, Minions.skurge]
      dealEachOtherPlayer
      Gnipahellir <$> runMessage msg attrs
    _ -> Gnipahellir <$> runMessage msg attrs

-- | 3 threat. Hinder 1[per hero], victory 3, an amplify icon (its card's).
gjallerbru :: SideSchemeCard Gjallerbru
gjallerbru = sideScheme Gjallerbru Cards.gjallerbru (Static 3)

newtype Gjallerbru = Gjallerbru (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Skurge: "Threat cannot be removed from Gjallerbru."
instance HasModifiersFor Gjallerbru where
  getModifiersFor _ target (Gjallerbru a) = guardedBy Minions.skurge a target

-- | When Defeated: the first player reveals Hall of Nastrond and Nidhogg, and puts them
-- into play. Deal each other player 1 facedown encounter card.
instance RunMessage Gjallerbru where
  runMessage msg (Gjallerbru attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      revealSetAside [Cards.hallOfNastrond, Minions.nidhogg]
      dealEachOtherPlayer
      Gjallerbru <$> runMessage msg attrs
    _ -> Gjallerbru <$> runMessage msg attrs

-- | 4 threat. Hinder 1[per hero], victory 4, a hazard icon (its card's).
hallOfNastrond :: SideSchemeCard HallOfNastrond
hallOfNastrond = sideScheme HallOfNastrond Cards.hallOfNastrond (Static 4)

newtype HallOfNastrond = HallOfNastrond (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Nidhogg: "Threat cannot be removed from Hall of Nastrond."
instance HasModifiersFor HallOfNastrond where
  getModifiersFor _ target (HallOfNastrond a) = guardedBy Minions.nidhogg a target

-- | When Defeated: the first player detaches Odin from the main scheme and takes control
-- of him (Odin's Torment does). Deal each player 1 facedown encounter card.
instance RunMessage HallOfNastrond where
  runMessage msg (HallOfNastrond attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      first <- selectJust FirstPlayer
      schemes <- selectList MainScheme
      for_ schemes \s -> cardSpecific (schemeRef s) "detachOdin" first
      traverse_ dealEncounterCard =<< getPlayers
      HallOfNastrond <$> runMessage msg attrs
    _ -> HallOfNastrond <$> runMessage msg attrs

-- | 2 threat, a crisis icon.
legionsOfHelScheme :: SideSchemeCard LegionsOfHelScheme
legionsOfHelScheme = sideSchemeWith LegionsOfHelScheme Cards.legionsOfHel (Static 2) (crisisL .~ True)

newtype LegionsOfHelScheme = LegionsOfHelScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: place 2 additional threat here for each Undead minion in play. If
-- there are no Undead minions in play, this card gains surge.
instance RunMessage LegionsOfHelScheme where
  runMessage msg s@(LegionsOfHelScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      undead <- length <$> (inAreaOf attrs . map MinionRef =<< inAnyArea (selectList (MinionWithTrait Undead)))
      if undead == 0
        then surge =<< getYou
        else placeThreat ident (2 * fromIntegral undead)
      pure s
    _ -> LegionsOfHelScheme <$> runMessage msg attrs

-- | 2[per hero] threat.
unnaturalStorm :: SideSchemeCard UnnaturalStorm
unnaturalStorm = sideScheme UnnaturalStorm Cards.unnaturalStorm (PerPlayer 2)

newtype UnnaturalStorm = UnnaturalStorm (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Heroes and allies cannot be readied by player card effects: those of its area.
instance HasModifiersFor UnnaturalStorm where
  getModifiersFor _ target (UnnaturalStorm a) = case target of
    IdentityRef _ -> inArea
    AllyRef _ -> inArea
    _ -> pure []
   where
    inArea = (\xs -> [CannotBeReadiedByPlayerCards | notNull xs]) <$> inAreaOf a [target]

-- | When Revealed: exhaust each ally in play.
instance RunMessage UnnaturalStorm where
  runMessage msg s@(UnnaturalStorm attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      traverse_ exhaust =<< inAreaOf attrs =<< inAnyArea (selectList AnyAlly)
      pure s
    _ -> UnnaturalStorm <$> runMessage msg attrs
