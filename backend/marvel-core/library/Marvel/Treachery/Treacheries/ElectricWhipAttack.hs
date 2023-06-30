module Marvel.Treachery.Treacheries.ElectricWhipAttack (
  electricWhipAttack,
  ElectricWhipAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

electricWhipAttack :: TreacheryCard ElectricWhipAttack
electricWhipAttack = treachery ElectricWhipAttack Cards.electricWhipAttack

newtype ElectricWhipAttack = ElectricWhipAttack (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage ElectricWhipAttack where
  runMessage msg t@(ElectricWhipAttack attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        upgrades <- selectList (UpgradeControlledBy $ IdentityWithId identityId)
        push $
          Ask identityId $
            ChooseOne
              [ Label
                  "Deal 1 damage to your hero for each upgrade you control"
                  [ DamageCharacter
                      (IdentityCharacter identityId)
                      (toRef attrs)
                      (toDamage (fromIntegral $ length upgrades) FromTreachery)
                  ]
              , Label
                  "Discard an upgrade you control"
                  [ ChooseUpgrade
                      (UpgradeControlledBy $ IdentityWithId identityId)
                      (toTarget attrs)
                  ]
              ]
        pure t
      _ -> ElectricWhipAttack <$> runMessage msg attrs
    ChoseUpgrade upgradeId target
      | isTarget attrs target ->
          t <$ push (RemoveFromPlay $ toRef upgradeId)
    Boost msg' -> case msg' of
      RevealedAsBoost target enemyId | isTarget attrs target -> do
        undefended <-
          member enemyId
            <$> select (UndefendedEnemy <> VillainEnemy)
        when undefended $ do
          ident <- getActivePlayerId
          pushChoice ident $
            ChooseUpgrade
              (UpgradeControlledBy $ IdentityWithId ident)
              target
        pure t
      _ -> ElectricWhipAttack <$> runMessage msg attrs
    _ -> ElectricWhipAttack <$> runMessage msg attrs
