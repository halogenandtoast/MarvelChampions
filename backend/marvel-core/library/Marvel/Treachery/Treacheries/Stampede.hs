module Marvel.Treachery.Treacheries.Stampede
  ( stampede
  , Stampede(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

stampede :: TreacheryCard Stampede
stampede = treachery Stampede Cards.stampede

newtype Stampede = Stampede TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Stampede where
  runMessage msg t@(Stampede attrs) = case msg of
    TreacheryMessage tid msg' | tid == toId attrs -> case msg' of
      RevealTreachery ident -> do
        isHero <- identityMatches HeroIdentity ident
        t <$ if isHero
          then do
            villainId <- selectJust ActiveVillain
            pushAll
              [ VillainMessage villainId $ VillainAttacks ident
              , RemoveFromPlay (toTarget attrs)
              ]
          else pushAll [RemoveFromPlay (toTarget attrs), Surge ident]
    _ -> Stampede <$> runMessage msg attrs
