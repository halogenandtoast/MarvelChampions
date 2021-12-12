module Marvel.Treachery.Treacheries.ElectricWhipAttack (
  electricWhipAttack,
  ElectricWhipAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

electricWhipAttack :: TreacheryCard ElectricWhipAttack
electricWhipAttack = treachery ElectricWhipAttack Cards.electricWhipAttack

newtype ElectricWhipAttack = ElectricWhipAttack TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage ElectricWhipAttack where
  runMessage msg t@(ElectricWhipAttack attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery ident -> do
          upgrades <- selectList (UpgradeControlledBy $ IdentityWithId ident)
          push $
            Ask ident $
              ChooseOne
                [ Label
                    "Deal 1 damage to your hero for each upgrade you control"
                    [ DamageCharacter
                        (IdentityCharacter ident)
                        (toSource attrs)
                        (fromIntegral $ length upgrades)
                    ]
                , Label
                    "Discard an upgrade you control"
                    [ ChooseUpgrade
                        (UpgradeControlledBy $ IdentityWithId ident)
                        (toTarget attrs)
                    ]
                ]
          pure t
        _ -> ElectricWhipAttack <$> runMessage msg attrs
    ChoseUpgrade upgradeId target | isTarget attrs target ->
      t <$ push (RemoveFromPlay $ UpgradeTarget upgradeId)
    Boost msg' -> case msg' of
      RevealedAsBoost target enemyId | isTarget attrs target -> do
        undefended <- member enemyId <$> select UndefendedEnemy
        when undefended $ do
          ident <- getActivePlayerId
          pushChoice ident (ChooseUpgrade (UpgradeControlledBy $ IdentityWithId ident) target)
        pure t
      _ -> ElectricWhipAttack <$> runMessage msg attrs
    _ -> ElectricWhipAttack <$> runMessage msg attrs
