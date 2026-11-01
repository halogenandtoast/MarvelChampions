module Marvel.Treachery.Treacheries.AgeOfApocalypseClanAkkaba where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Question ()
import Marvel.Scenario.AgeOfApocalypseEnSabahNur
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaClanAkkabaTreachery = AoaClanAkkabaTreachery (Attrs Treachery `With` Bool)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

tyrantWorship :: TreacheryCard AoaClanAkkabaTreachery
tyrantWorship = treachery (AoaClanAkkabaTreachery . (`With` False)) Cards.tyrantWorship

instance IsTreachery AoaClanAkkabaTreachery where
  toTreacheryAttrs (AoaClanAkkabaTreachery (attrs `With` _)) = attrs

instance HasCardCode AoaClanAkkabaTreachery where
  toCardCode (AoaClanAkkabaTreachery (attrs `With` _)) = toCardCode attrs

instance IsRef AoaClanAkkabaTreachery where
  toRef (AoaClanAkkabaTreachery (attrs `With` _)) = toRef attrs
instance RunMessage AoaClanAkkabaTreachery where
  runMessage msg a@(AoaClanAkkabaTreachery (attrs `With` extra)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId attrs -> a <$ placeAncientRitualThreat 5
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      chooseOne you
        [labeled (countLabel "aoaPlaceAncientRitualThreat" 3) $ placeAncientRitualThreat 3,
         labeled "$label.aoaThreeBoostIcons" $ cardSpecific attrs "extraBoostIcons" ()]
      pure a
    Boost (CardSpecific target "extraBoostIcons" _) | isTarget attrs target -> pure $ AoaClanAkkabaTreachery $ attrs `With` True
    Boost (CheckWindows windows) | extra -> do
      for_ [card | W.Window _ (W.CountingBoostIcons card) <- windows, toCardCode card == toCardCode attrs] $ \card -> changeBoostIconCount card (+3)
      pure a
    _ -> AoaClanAkkabaTreachery . (`With` extra) <$> runMessage msg attrs
