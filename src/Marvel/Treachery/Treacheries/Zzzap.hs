module Marvel.Treachery.Treacheries.Zzzap (zzzap, Zzzap (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Minion.Minions.Zzzax (handEnergy)
import Marvel.Window qualified as W
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype Meta = Meta {identityDamageDealt :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Zzzap = Zzzap (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

zzzap :: TreacheryCard Zzzap
zzzap = treachery (Zzzap . (`With` Meta 0)) Cards.zzzap

instance IsTreachery Zzzap where
  toTreacheryAttrs (Zzzap (attrs `With` _)) = attrs

instance RunMessage Zzzap where
  runMessage msg t@(Zzzap (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      n <- handEnergy you
      when (n > 0) $ dealIndirectDamage you attrs n
      cardSpecific attrs "checkSurge" ()
      pure $ Zzzap (attrs {treacheryResolver = Just you} `With` Meta 0)
    CheckWindows windows -> do
      let dealt = sum [damageAmount damage | W.Window W.When (W.IdentityTakeDamageFrom you source damage) <- windows
                     , source == toRef attrs, treacheryResolver attrs == Just you]
      Zzzap . (`With` Meta (identityDamageDealt meta + dealt)) <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "checkSurge" _ ->
      pure $ Zzzap ((attrs & surgeL .~ (identityDamageDealt meta <= 1)) `With` meta)
    _ -> Zzzap . (`With` meta) <$> runMessage msg attrs
