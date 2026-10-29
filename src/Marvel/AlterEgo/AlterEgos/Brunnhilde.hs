module Marvel.AlterEgo.AlterEgos.Brunnhilde (brunnhilde, Brunnhilde (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Upgrades

brunnhilde :: AlterEgoCard Brunnhilde
brunnhilde = alterEgo Brunnhilde Cards.brunnhilde (HP $ Static 12) (HandSize 6) (Rec 4) [Obligations.troubleInOtherworld]

newtype Brunnhilde = Brunnhilde (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities Brunnhilde where
  getAbilities a@(Brunnhilde attrs) =
    [ ability a 1 Setup NoCriteria NoCost (runAbility a 1)
    , label "$label.notThisDay" $ ability a 2 Action
        (IsSelf <> UpgradeExists (UpgradeIs Upgrades.deathGlow <> upgradeControlledBy (alterEgoIdentityId attrs)))
        NoCost (runAbility a 2)
    ]

instance RunMessage Brunnhilde where
  runMessage msg ae@(Brunnhilde attrs) = runQueueT $ case msg of
    -- Setup abilities are resolved after the opening hand/mulligan; no replacement
    -- card is drawn if Death-Glow was among those kept.
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> ae <$ setAsideOwnedCard ident "25002"
    RanAbility ident (isTarget attrs -> True) 2 _ _ -> do
      glows <- selectList (UpgradeIs Upgrades.deathGlow <> upgradeControlledBy ident)
      for_ glows $ \uid -> do
        card <- getsGame (`cardInPlay` UpgradeRef uid)
        for_ card (setAside . pure)
      pure ae
    _ -> Brunnhilde <$> runMessage msg attrs
