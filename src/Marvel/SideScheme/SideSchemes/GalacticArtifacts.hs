-- | The Galactic Artifacts modular set's side schemes (The Galaxy's Most Wanted): each
-- base threat 3, Hinder 2 per player and Victory 0 (its card's keywords), and a "When
-- Defeated" for the player who defeated it, who may take it or not.
module Marvel.SideScheme.SideSchemes.GalacticArtifacts (
  hujahdarianMonarchEgg,
  HujahdarianMonarchEgg (..),
  magicalTeapot,
  MagicalTeapot (..),
  philosophersStone,
  PhilosophersStone (..),
  crystalBall,
  CrystalBall (..),
  crystalBallEffect,
  CrystalBallEffect (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | "The player who defeated this scheme may …": what they may do, or not.
whenDefeatedMay :: (ReverseQueue m) => Attrs SideScheme -> Text -> (IdentityId -> m ()) -> m ()
whenDefeatedMay attrs label body = do
  who <- getDefeatedBy attrs
  chooseOne who [labeled label (body who), labeled "$label.pass" (pure ())]

hujahdarianMonarchEgg :: SideSchemeCard HujahdarianMonarchEgg
hujahdarianMonarchEgg = sideScheme HujahdarianMonarchEgg Cards.hujahdarianMonarchEgg (Static 3)

-- | When Defeated: the player who defeated this scheme may ready their identity.
newtype HujahdarianMonarchEgg = HujahdarianMonarchEgg (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage HujahdarianMonarchEgg where
  runMessage msg (HujahdarianMonarchEgg attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      whenDefeatedMay attrs "$label.readyIdentity" readyByGame
      HujahdarianMonarchEgg <$> runMessage msg attrs
    _ -> HujahdarianMonarchEgg <$> runMessage msg attrs

magicalTeapot :: SideSchemeCard MagicalTeapot
magicalTeapot = sideScheme MagicalTeapot Cards.magicalTeapot (Static 3)

-- | When Defeated: the player who defeated this scheme may heal 4 damage from their
-- identity. Its hazard icon is its card's.
newtype MagicalTeapot = MagicalTeapot (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage MagicalTeapot where
  runMessage msg (MagicalTeapot attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      whenDefeatedMay attrs (countLabel "healYourIdentity" 4) (`heal` 4)
      MagicalTeapot <$> runMessage msg attrs
    _ -> MagicalTeapot <$> runMessage msg attrs

philosophersStone :: SideSchemeCard PhilosophersStone
philosophersStone = sideScheme PhilosophersStone Cards.philosophersStone (Static 3)

-- | When Defeated: the player who defeated this scheme may draw 2 cards. Its amplify icon
-- is its card's.
newtype PhilosophersStone = PhilosophersStone (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage PhilosophersStone where
  runMessage msg (PhilosophersStone attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      whenDefeatedMay attrs (countLabel "drawCards" 2) (`drawCards` 2)
      PhilosophersStone <$> runMessage msg attrs
    _ -> PhilosophersStone <$> runMessage msg attrs

crystalBall :: SideSchemeCard CrystalBall
crystalBall = sideScheme CrystalBall Cards.crystalBall (Static 3)

-- | When Defeated: the player who defeated this scheme may play a card from their hand,
-- reducing its resource cost by 3. The reduction is an effect on them, gone once they
-- play a card or decline to; the cards offered are those they may play now with it
-- (`AffordableCardBy` is the full play check: timing, criteria, limits, unique).
newtype CrystalBall = CrystalBall (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage CrystalBall where
  runMessage msg s@(CrystalBall attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      who <- getDefeatedBy attrs
      createEffect Cards.crystalBall attrs (IdentityEntity $ IdentityWithId who)
      cardSpecific attrs "play" who
      CrystalBall <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "play" (fromJSON -> Success who) -> do
      cards <- selectList $ InHandOf (IdentityWithId who) $ AffordableCardBy (IdentityWithId who) (BasicCardMatches AnyCard)
      chooseOne who $
        labeled "$label.pass" (cardSpecific attrs "done" ())
          : [pure (PlayCard c Nothing) | c <- cards]
      pure s
    _ -> CrystalBall <$> runMessage msg attrs

newtype CrystalBallEffect = CrystalBallEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

crystalBallEffect :: CardEffect CrystalBallEffect
crystalBallEffect =
  effectWith CrystalBallEffect Cards.crystalBall $
    (modifiersL .~ [ResourceCostReduction 3]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage CrystalBallEffect where
  runMessage msg e@(CrystalBallEffect attrs) = runQueueT $ case msg of
    IdentityMessage ident (PlayedCard _ _) -> do
      whenM (effectValidFor attrs (toRef ident)) $ disableEffect (effectId attrs)
      pure e
    -- no card played after all
    CardSpecific target "done" _ | target == refocus (effectSource attrs) -> do
      disableEffect (effectId attrs)
      pure e
    _ -> CrystalBallEffect <$> runMessage msg attrs
