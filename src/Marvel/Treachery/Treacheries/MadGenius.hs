module Marvel.Treachery.Treacheries.MadGenius (
  madGenius,
  MadGenius (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Environment.Environments.CriminalEnterprise (getInfamy, isGreenGoblinSide, isNormanOsbornSide)

madGenius :: TreacheryCard MadGenius
madGenius = treachery (MadGenius . (`With` Meta False)) Cards.madGenius

newtype Meta = Meta {attackMade :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MadGenius = MadGenius (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery MadGenius where
  toTreacheryAttrs (MadGenius (attrs `With` _)) = attrs

instance RunMessage MadGenius where
  runMessage msg t@(MadGenius (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident msg' | ident == treacheryId attrs -> case msg' of
      RevealTreachery you -> do
        -- (Green Goblin): he attacks the hero with the fewest hit points remaining
        goblin <- isGreenGoblinSide
        when goblin do
          villain <- selectJust ActiveVillain
          heroes <- selectList (IdentityWithFewestRemainingHitPoints HeroIdentity)
          unless (null heroes) $ chooseOrRunOne you [targeting hero (enemyAttacks villain hero) | hero <- heroes]
        -- (Norman Osborn): the top card of your deck for each infamy counter
        norman <- isNormanOsbornSide
        when norman do
          infamy <- getInfamy
          when (infamy > 0) $ discardTopOfDeck you infamy
        pure t
      -- "If no attack was made this way, this card gains surge"
      CheckTreacheryCondition _ -> do
        goblin <- isGreenGoblinSide
        pure . MadGenius $ (if goblin && not (attackMade meta) then attrs & surgeL .~ True else attrs) `With` meta
      _ -> MadGenius . (`With` meta) <$> runMessage msg attrs
    VillainMessage _ VillainAttacked -> pure $ MadGenius (attrs `With` Meta True)
    _ -> MadGenius . (`With` meta) <$> runMessage msg attrs
