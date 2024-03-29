module Marvel.Treachery.Treacheries.ElectromagneticBacklash (
  electromagneticBacklash,
  ElectromagneticBacklash (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

electromagneticBacklash :: TreacheryCard ElectromagneticBacklash
electromagneticBacklash =
  treachery ElectromagneticBacklash Cards.electromagneticBacklash

newtype ElectromagneticBacklash = ElectromagneticBacklash (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage ElectromagneticBacklash where
  runMessage msg t@(ElectromagneticBacklash attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery _ -> do
        players <- getPlayers
        pushAll $
          map
            ( \i ->
                IdentityMessage i $ DiscardFrom FromDeck 5 (Just $ toTarget attrs)
            )
            players
        pure t
      _ -> ElectromagneticBacklash <$> runMessage msg attrs
    WithDiscarded target _ (onlyPlayerCards -> cs) | isTarget attrs target ->
      case cs of
        [] -> pure t
        (x : _) -> case pcOwner x of
          Nothing -> error "Unexpected"
          Just ident -> do
            let
              energyCount =
                count (== Energy) $ concatMap (printedResources . getCardDef) cs
            when (energyCount > 0) $
              push $
                IdentityMessage ident $
                  IdentityDamaged
                    (toSource attrs)
                    (toDamage energyCount FromTreachery)
            pure t
    _ -> ElectromagneticBacklash <$> runMessage msg attrs
