module Marvel.Treachery.Treacheries.ElectricWhipAttack (
  electricWhipAttack,
  ElectricWhipAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

electricWhipAttack :: TreacheryCard ElectricWhipAttack
electricWhipAttack = treachery ElectricWhipAttack Cards.electricWhipAttack

newtype ElectricWhipAttack = ElectricWhipAttack (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage ElectricWhipAttack where
  runMessage msg t@(ElectricWhipAttack attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        upgrades <- selectList (UpgradeControlledBy $ IdentityWithId identityId)
        chooseOne identityId
          [ labeled "$label.deal1DamageToYourHeroForEachUpgradeYouControl" $
              damageCharacter identityId attrs (toDamage (fromIntegral $ length upgrades) FromTreachery)
          , labeled "$label.discardAnUpgradeYouControl" $
              chooseUpgrade identityId (UpgradeControlledBy $ IdentityWithId identityId) attrs
          ]
        pure t
      _ -> ElectricWhipAttack <$> runMessage msg attrs
    ChoseUpgrade upgradeId target
      | isTarget attrs target ->
          t <$ removeFromPlay upgradeId
    Boost msg' -> case msg' of
      RevealedAsBoost target enemyId | isTarget attrs target -> do
        undefended <-
          member enemyId
            <$> select (UndefendedEnemy <> VillainEnemy)
        when undefended $ do
          ident <- getActivePlayerId
          chooseUpgrade ident (UpgradeControlledBy $ IdentityWithId ident) target
        pure t
      -- turned up as a boost, it hears of the upgrade chosen from the boost area
      ChoseUpgrade upgradeId target
        | isTarget attrs target ->
            t <$ removeFromPlay upgradeId
      _ -> ElectricWhipAttack <$> runMessage msg attrs
    _ -> ElectricWhipAttack <$> runMessage msg attrs
