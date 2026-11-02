{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50020 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

aosP50020 :: SupportCard AosP50020
aosP50020 = supportWith AosP50020 Cards.aosP50020 ((usesL .~ 2) . (discardIfNoUsesL .~ True))

newtype AosP50020 = AosP50020 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50020 where
  getAbilities (AosP50020 attrs) =
    [ability attrs 1 Action (OwnsThis) (ExhaustCost <> UseCost) (runAbility attrs 1)]

instance RunMessage AosP50020 where
  runMessage msg card@(AosP50020 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      heroes <- selectList HeroIdentity
      villains <- selectList AnyVillain
      chooseOne you $ [targeting p (cardSpecific attrs "chooseStatus" (IdentityCharacter p)) | p <- heroes]
        <> [targeting v (cardSpecific attrs "chooseStatus" (VillainCharacter v)) | v <- villains]
      allies <- selectList AnyAlly
      minions <- selectList AnyMinion
      chooseOne you $ [targeting a (cardSpecific attrs "chooseStatus" (AllyCharacter a)) | a <- allies]
        <> [targeting m (cardSpecific attrs "chooseStatus" (MinionCharacter m)) | m <- minions]

      pure card
    CardSpecific (isTarget attrs -> True) "chooseStatus" (fromJSON -> Success (target :: CharacterId)) -> do
      let you = supportController attrs
      chooseOne you [labeled "$label.stun" (giveStatus target StunnedStatus),labeled "$label.confuse" (giveStatus target ConfusedStatus),labeled "$label.tough" (giveStatus target ToughStatus)]
      pure card
    _ -> AosP50020 <$> runMessage msg attrs
