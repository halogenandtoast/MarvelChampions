module Marvel.Support.Supports.SanctumSanctorum (
  sanctumSanctorum,
  SanctumSanctorum (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Message
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait

sanctumSanctorum :: SupportCard SanctumSanctorum
sanctumSanctorum = support SanctumSanctorum Cards.sanctumSanctorum

newtype SanctumSanctorum = SanctumSanctorum (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SanctumSanctorum where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost $ runAbility a 1]

-- | Shuffle a Spell card from your discard pile into your deck, and draw 1 card.
instance RunMessage SanctumSanctorum where
  runMessage msg s@(SanctumSanctorum attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      spells <- filter (cardMatch $ CardWithTrait Spell) <$> projectMap PlayerIdentityDiscard unDiscard ident
      let draw = IdentityMessage ident (DrawCards FromDeck 1)
      push $
        Ask ident $
          prompted "$label.chooseSpellToShuffle" $
            ChooseOne $
              [ TargetLabel (toRef c) [Run [IdentityMessage ident (ShuffleIntoIdentityDeck [c]), draw]]
              | c <- spells
              ]
                <> [Label "$label.drawCard" [Run [draw]] | null spells]
      pure s
    _ -> SanctumSanctorum <$> runMessage msg attrs
