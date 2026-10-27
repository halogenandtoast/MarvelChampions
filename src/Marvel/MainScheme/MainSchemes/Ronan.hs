-- | Ronan the Accuser's main schemes (The Galaxy's Most Wanted, scenario 5).
module Marvel.MainScheme.MainSchemes.Ronan (
  interceptionImminent,
  InterceptionImminent (..),
  takeWhatIsMine,
  TakeWhatIsMine (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id (CharacterId (..))
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref

interceptionImminent :: MainSchemeCard InterceptionImminent
interceptionImminent =
  mainScheme InterceptionImminent Cards.interceptionImminent (PerPlayer 7) (PerPlayer 2) (PerPlayer 2)

newtype InterceptionImminent = InterceptionImminent (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1B. First Player Action: exhaust the Milano → remove 3 threat from this scheme.
instance IsMainScheme InterceptionImminent where
  mainSchemeAbilities (InterceptionImminent a) =
    [ability a 1 Action (SelfMatches FirstPlayer) ExhaustMilanoCost $ runAbility a 1]

-- | 1A Setup: put the Kree Command Ship environment and the Milano support into play
-- (the Milano, with the setup keyword, is in play already), attach the Universal Weapon
-- to Ronan the Accuser, and attach the Power Stone to the first player.
instance RunMessage InterceptionImminent where
  runMessage msg ms@(InterceptionImminent attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      searchAndPutIntoPlayEncounterCard first (CardIs Cards.kreeCommandShip)
      searchAndPutIntoPlayEncounterCard first (CardIs Cards.universalWeapon)
      attachPowerStoneTo (IdentityCharacter first)
      pure ms
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> ms <$ thwartScheme (mainSchemeId attrs) attrs 3
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    _ -> InterceptionImminent <$> runMessage msg attrs

takeWhatIsMine :: MainSchemeCard TakeWhatIsMine
takeWhatIsMine =
  mainScheme TakeWhatIsMine Cards.takeWhatIsMine (PerPlayer 10) (PerPlayer 1) (PerPlayer 2)

newtype TakeWhatIsMine = TakeWhatIsMine (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 2B: while the Power Stone is attached to Ronan the Accuser, threat cannot be removed
-- from this scheme.
instance IsMainScheme TakeWhatIsMine where
  mainSchemeModifiersFor _ (MainSchemeRef mid) (TakeWhatIsMine a) | mid == mainSchemeId a = do
    holder <- getPowerStoneHolder
    pure [ThreatCannotBeRemoved | Just (VillainCharacter _) <- [holder]]
  mainSchemeModifiersFor _ _ _ = pure []

-- | 2A When Revealed: attach the Power Stone to Ronan the Accuser; if it is already
-- attached to him, give him 1 facedown boost card. 2B completed, the players lose.
instance RunMessage TakeWhatIsMine where
  runMessage msg ms@(TakeWhatIsMine attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      holder <- getPowerStoneHolder
      selectOne ActiveVillain >>= traverse_ \villain ->
        if holder == Just (VillainCharacter villain)
          then dealBoost villain
          else attachPowerStoneTo villain
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> TakeWhatIsMine <$> runMessage msg attrs
