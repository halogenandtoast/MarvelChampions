module Marvel.Event.Events.IntoTheFray (
  intoTheFray,
  IntoTheFray (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

intoTheFray :: EventCard IntoTheFray
intoTheFray = event (IntoTheFray . (`With` Meta False)) Cards.intoTheFray

-- | Whether the threat for the excess damage has been removed: the window it answers
-- opens again after each ability used in it, and is answered once.
newtype Meta = Meta {excessResolved :: Bool}
  deriving stock (Show, Eq)

instance ToJSON Meta where
  toJSON m = object ["excessResolved" .= excessResolved m]

instance FromJSON Meta where
  parseJSON = withObject "Meta" \o -> Meta . fromMaybe False <$> o .:? "excessResolved"

newtype IntoTheFray = IntoTheFray (With (Attrs Event) Meta)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent IntoTheFray where
  toEventAttrs (IntoTheFray (With attrs _)) = attrs

-- | Hero Action (attack): Deal 6 damage to a minion. For each point of excess damage
-- dealt by this attack, remove 1 threat from the main scheme. The excess is what this
-- attack dealt past the minion's hit points, known as its defeat is checked. No threat is
-- removed while a crisis icon is in play.
instance RunMessage IntoTheFray where
  runMessage msg e@(IntoTheFray (With attrs meta)) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      minions <- selectList (AttackableEnemy <> MinionEnemy)
      chooseOne you
        [ targeting (enemyRef minion) $ dealDamage minion attrs (toDamage 6 $ FromPlayerAttack you)
        | minion <- minions
        ]
      pure e
    CheckWindows [W.Window W.When (W.DefeatedMinion _ _ source excess)]
      | source == toSource attrs && excess > 0 && not (excessResolved meta) -> do
          mains <- filter isMain <$> selectList ThwartableScheme
          -- none to remove it from (a crisis icon in play): nothing happens
          unless (null mains) $ chooseOrRunOne (eventController attrs)
            [targeting (schemeRef scheme) (thwartScheme scheme attrs excess) | scheme <- mains]
          attrs' <- runMessage msg attrs
          pure $ IntoTheFray (With attrs' (Meta True))
    _ -> IntoTheFray . (`With` meta) <$> runMessage msg attrs
   where
    isMain = \case
      SchemeMainSchemeId _ -> True
      _ -> False
