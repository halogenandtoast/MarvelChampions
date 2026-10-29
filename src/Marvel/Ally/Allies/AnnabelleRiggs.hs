module Marvel.Ally.Allies.AnnabelleRiggs (annabelleRiggs, AnnabelleRiggs (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria

annabelleRiggs :: AllyCard AnnabelleRiggs
annabelleRiggs = ally AnnabelleRiggs Cards.annabelleRiggs (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype AnnabelleRiggs = AnnabelleRiggs (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AnnabelleRiggs where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

-- The title restriction is intentional: if another identity controls Annabelle,
-- she still finds Valkyrie cards, rather than that identity's signature cards.
instance RunMessage AnnabelleRiggs where
  runMessage msg a@(AnnabelleRiggs attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      search ident (SearchIdentityDeck ident $ TopOfDeck 5)
        (HeroSpecificCard <> CardMatchesAny (map CardWithTitle
          ["Death-Glow", "Annabelle Riggs", "Valhalla", "Valkyrie's Spear", "Dragonfang", "Aragorn", "Flight of the Valkyrior", "Visit Valhalla", "Chooser of the Slain", "Shieldmaiden", "Have at Thee!"]))
        SearchDrawOne ShuffleBackIn
      pure a
    _ -> AnnabelleRiggs <$> runMessage msg attrs
