module Marvel.SideScheme.SideSchemes.UnderAttack (
  underAttack,
  UnderAttack (..),
) where

import Marvel.Prelude

import Marvel.Id (SchemeId (..))

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

underAttack :: SideSchemeCard UnderAttack
underAttack = sideSchemeWith UnderAttack Cards.underAttack (Static 3) (crisisL .~ True)

newtype UnderAttack = UnderAttack (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage UnderAttack where
  runMessage msg s@(UnderAttack attrs) = runQueueT $ case msg of
    -- each player places 2 threat here or deals 3 damage to their hero; a player in
    -- alter-ego form has no hero to damage
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      heroes <- selectList HeroIdentity
      for_ players \p ->
        chooseOne p $
          labeled (countLabel "placeThreatOnUnderAttack" 2) (placeThreat ident 2)
            : [ labeled (countLabel "dealDamageToYourHero" 3) (damageIdentity p attrs (toDamage 3 FromAbility))
              | p `elem` heroes
              ]
      pure s
    _ -> UnderAttack <$> runMessage msg attrs
