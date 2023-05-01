module Marvel.Treachery.Treacheries.Explosion (
  explosion,
  Explosion (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Count
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
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

explosion :: TreacheryCard Explosion
explosion = treachery Explosion Cards.explosion

newtype Explosion = Explosion (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Explosion where
  runMessage msg t@(Explosion attrs) = case msg of
    TreacheryMessage ident msg' | ident == treacheryId attrs -> case msg' of
      RevealTreachery identityId -> do
        mBombScare <- selectOne $ SideSchemeIs Cards.bombScare
        case mBombScare of
          Nothing -> pure . Explosion $ attrs & surgeL .~ True
          Just bombScare -> do
            threat <-
              fromIntegral
                <$> selectCount
                  SchemeThreat
                  (SchemeWithId $ SchemeSideSchemeId bombScare)
            players <- getPlayers
            allies <- selectList AnyAlly
            pushAll $
              replicate threat $
                Ask identityId $
                  ChooseOne $
                    [ DamageCharacter
                      (IdentityCharacter iid)
                      (toSource attrs)
                      (toDamage 1 FromTreachery)
                    | iid <- players
                    ]
                      <> [ DamageCharacter
                          (AllyCharacter aid)
                          (toSource attrs)
                          (toDamage 1 FromTreachery)
                         | aid <- allies
                         ]
            pure t
      _ -> Explosion <$> runMessage msg attrs
    _ -> Explosion <$> runMessage msg attrs
