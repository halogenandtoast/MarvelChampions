module Marvel.Treachery.Treacheries.PiercingThorns (
  piercingThorns,
  PiercingThorns (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

piercingThorns :: TreacheryCard PiercingThorns
piercingThorns = treachery PiercingThorns Cards.piercingThorns

newtype PiercingThorns = PiercingThorns (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Discard 1 card at random from your hand; if Absorbing Man has the Wood trait, discard
-- 1 card you control. [star] Boost: if he has the Stone or Wood trait, you are stunned.
instance RunMessage PiercingThorns where
  runMessage msg t@(PiercingThorns attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardFrom you RandomFromHand 1
      wood <- selectAny $ VillainWithTrait Wood
      when wood do
        let you' = IdentityWithId you
        allies <- selectList $ AllyControlledBy you'
        supports <- selectList $ SupportControlledBy you'
        upgrades <- selectList $ UpgradeControlledBy you'
        let controlled = map toTarget allies <> map toTarget supports <> map toTarget upgrades
        unless (null controlled) $ chooseOrRunOne you [targeting c (removeFromPlay c) | c <- controlled]
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      stoneOrWood <- orM [selectAny (VillainWithTrait Stone), selectAny (VillainWithTrait Wood)]
      when stoneOrWood $ stunIdentity =<< getBoostedPlayer enemy
      pure t
    _ -> PiercingThorns <$> runMessage msg attrs
