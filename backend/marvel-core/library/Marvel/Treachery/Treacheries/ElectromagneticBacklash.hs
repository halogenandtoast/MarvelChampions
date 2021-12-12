module Marvel.Treachery.Treacheries.ElectromagneticBacklash (
  electromagneticBacklash,
  ElectromagneticBacklash (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

electromagneticBacklash :: TreacheryCard ElectromagneticBacklash
electromagneticBacklash =
  treachery ElectromagneticBacklash Cards.electromagneticBacklash

newtype ElectromagneticBacklash = ElectromagneticBacklash TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage ElectromagneticBacklash where
  runMessage msg t@(ElectromagneticBacklash attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery _ -> do
          players <- getPlayers
          pushAll $
            map
              ( \i ->
                  IdentityMessage i $
                    DiscardFrom FromDeck 5 (Just $ toTarget attrs)
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
            let x =
                  count (== Energy) $ concatMap (printedResources . getCardDef) cs
            when
              (x > 0)
              ( push $ IdentityMessage ident (IdentityDamaged (toSource attrs) x)
              )
            pure t
    _ -> ElectromagneticBacklash <$> runMessage msg attrs
