module Marvel.Hero.Heroes.WaspTiny (
  waspTiny,
  WaspTiny (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

waspTiny :: HeroCard WaspTiny
waspTiny =
  hero
    WaspTiny
    Cards.waspTiny
    (HP $ Static 11)
    (HandSize 5)
    (Thw 1)
    (Atk 1)
    (Def 2)

-- | Small but Mighty — Response: After Wasp (or an event you play) defeats a minion or
-- side scheme, deal 1 damage to the villain. "Wasp" is her basic powers and her hero's
-- own abilities, not her upgrades, supports or allies (owner ruling).
instance HasAbilities WaspTiny where
  getAbilities (WaspTiny a) =
    [ label "Small but Mighty" $
        limitedWindowAbility a 1 (W.MinionDefeatedBy W.After AnyMinion wasp) Response IsSelf NoCost $ runAbility a 1
    , label "Small but Mighty" $
        limitedWindowAbility a 2 (W.SideSchemeDefeatedBy AnySideScheme wasp) Response IsSelf NoCost $ runAbility a 1
    ]
   where
    you = IdentityWithId $ heroIdentityId a
    wasp = SourceOneOf [SourceIsIdentity you, EventPlayedBy you]

newtype WaspTiny = WaspTiny (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage WaspTiny where
  runMessage msg h@(WaspTiny attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      withTheVillain you \villain -> dealDamage villain attrs (toDamage 1 FromAbility)
      pure h
    _ -> WaspTiny <$> runMessage msg attrs
