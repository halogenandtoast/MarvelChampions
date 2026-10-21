module Marvel.SideScheme.SideSchemes.UnderAttack (
  underAttack,
  UnderAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

underAttack :: SideSchemeCard UnderAttack
underAttack = sideSchemeWith UnderAttack Cards.underAttack (Static 3) (crisisL .~ True)

newtype UnderAttack = UnderAttack (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage UnderAttack where
  runMessage msg s@(UnderAttack attrs) = case msg of
    -- each player places 2 threat here or deals 3 damage to their hero; a player in
    -- alter-ego form has no hero to damage
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      heroes <- selectList HeroIdentity
      pushAll
        [ Ask p . ChooseOne $
          Label "$label.placeTwoThreatOnUnderAttack" [Run [SideSchemeMessage ident (SideSchemePlaceThreat 2)]]
            : [ Label
                "$label.deal3DamageToYourHero"
                [Run [IdentityMessage p $ IdentityDamaged (toSource attrs) (toDamage 3 FromAbility)]]
              | p `elem` heroes
              ]
        | p <- players
        ]
      pure s
    _ -> UnderAttack <$> runMessage msg attrs
