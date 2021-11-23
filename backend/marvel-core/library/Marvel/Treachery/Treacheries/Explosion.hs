module Marvel.Treachery.Treacheries.Explosion
  ( explosion
  , Explosion(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Count
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

explosion :: TreacheryCard Explosion
explosion = treachery Explosion Cards.explosion

newtype Explosion = Explosion TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Explosion where
  runMessage msg t@(Explosion attrs) = case msg of
    TreacheryMessage tid msg' | tid == toId attrs -> case msg' of
      RevealTreachery ident -> do
        mBombScare <- selectOne $ SideSchemeIs Cards.bombScare
        case mBombScare of
          Nothing -> push $ Surge ident
          Just bombScare -> do
            game <- getsGame id
            threat <-
              fromIntegral
                <$> runReaderT
                      (selectCount
                        SchemeThreat
                        (SchemeWithId $ SchemeSideSchemeId bombScare)
                      )
                      game
            players <- getPlayers
            allies <- selectList AnyAlly
            pushAll $ replicate threat $ Ask
              ident
              (ChooseOne
              $ [ DamageCharacter (IdentityCharacter iid) (toSource attrs) 1
                | iid <- players
                ]
              <> [ DamageCharacter (AllyCharacter aid) (toSource attrs) 1
                 | aid <- allies
                 ]
              )
        pure t
      _ -> Explosion <$> runMessage msg attrs
    _ -> Explosion <$> runMessage msg attrs
