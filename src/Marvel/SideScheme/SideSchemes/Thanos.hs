module Marvel.SideScheme.SideSchemes.Thanos (
  sanctuary,
  Sanctuary (..),
  theTitansThrone,
  TheTitansThrone (..),
  tribute,
  Tribute (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

sanctuary :: SideSchemeCard Sanctuary
sanctuary = sideScheme Sanctuary Cards.sanctuary (Static 3)

newtype Sanctuary = Sanctuary (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Thanos cannot take damage from player cards (errata).
-- ponytail: on any villain, as Sanctuary is only ever in play with Thanos
instance HasModifiersFor Sanctuary where
  getModifiersFor _ (VillainRef _) _ = pure [CannotTakeDamageFromPlayerCards]
  getModifiersFor _ _ _ = pure []

-- | When Defeated: each player may spend up to 3 [physical] resources from their hand.
-- Deal 2 damage to Thanos for each [physical] resource spent this way, ignoring the
-- tough status card: one effect, so one amount of 2 per resource spent by all the
-- players (RR For Each), the side scheme's, reduced once by Thanos's Armor. Each player
-- in turn decides, the running total carried from one to the next.
instance RunMessage Sanctuary where
  runMessage msg (Sanctuary attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      cardSpecific attrs "sanctuarySpend" (players, 0 :: Natural)
      Sanctuary <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "sanctuarySpend" (fromJSON -> Success (remaining :: [IdentityId], spent :: Natural)) -> do
      case remaining of
        [] -> when (spent > 0) do
          villain <- selectOne ActiveVillain
          for_ villain \v -> dealDamage v attrs (ignoringTough $ toDamage (2 * spent) FromAbility)
        p : rest -> do
          let cost n = ResourcesFromHandCost (MultiResourceCost (replicate (fromIntegral n) (Just Physical)))
              next n = cardSpecific attrs "sanctuarySpend" (rest, spent + n)
          affordable <- filterM (\n -> canAffordCost p (toSource attrs) (cost n)) [1 .. 3]
          if null affordable
            then next 0
            else
              chooseOnePrompted p "$label.sanctuarySpendPhysical" $
                labeled "$label.spendNothing" (next 0)
                  : [ labeled (countLabel "spendPhysicalResources" n) (payForTreachery p (cost n) >> next n)
                    | n <- affordable
                    ]
      pure $ Sanctuary attrs
    _ -> Sanctuary <$> runMessage msg attrs

theTitansThrone :: SideSchemeCard TheTitansThrone
theTitansThrone = sideScheme TheTitansThrone Cards.theTitansThrone (PerPlayer 2)

newtype TheTitansThrone = TheTitansThrone (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: choose and discard an Infinity Stone from play (an Infinity Stone
-- environment: the other Power Stone is a permanent attachment).
instance RunMessage TheTitansThrone where
  runMessage msg (TheTitansThrone attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      you <- getYou
      stones <- selectList (EnvironmentWithTrait InfinityStone)
      chooseOnePrompted you "$label.chooseInfinityStoneToDiscard" [targeting stone (removeFromPlay stone) | stone <- stones]
      TheTitansThrone <$> runMessage msg attrs
    _ -> TheTitansThrone <$> runMessage msg attrs

tribute :: SideSchemeCard Tribute
tribute = sideSchemeWith Tribute Cards.tribute (PerPlayer 2) (crisisL .~ True)

newtype Tribute = Tribute (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: deal the player who defeated this scheme a facedown encounter card.
instance RunMessage Tribute where
  runMessage msg (Tribute attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      dealEncounterCard =<< getDefeatedBy attrs
      Tribute <$> runMessage msg attrs
    _ -> Tribute <$> runMessage msg attrs
