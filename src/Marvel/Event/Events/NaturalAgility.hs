module Marvel.Event.Events.NaturalAgility (naturalAgility, NaturalAgility (..)) where

import Marvel.Prelude
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

naturalAgility :: EventCard NaturalAgility
naturalAgility = event (NaturalAgility . (`With` Meta Nothing)) Cards.naturalAgility

newtype Meta = Meta {defense :: Maybe (IdentityId, EnemyId, Natural)}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype NaturalAgility = NaturalAgility (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent NaturalAgility where
  toEventAttrs (NaturalAgility (attrs `With` _)) = attrs

instance IsRef NaturalAgility where
  toRef = toRef . toEventAttrs

instance HasModifiersFor NaturalAgility where
  getModifiersFor _ (IdentityRef you) (NaturalAgility (_ `With` Meta (Just (defender, _, n))))
    | you == defender = pure [DefenseModifier n]
  getModifiersFor _ _ _ = pure []

instance RunMessage NaturalAgility where
  runMessage msg e@(NaturalAgility (attrs `With` meta)) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.HeroDefends _ enemy))) | eid == eventId attrs -> do
      n <- getCountersOn you
      pure $ NaturalAgility (attrs `With` Meta (Just (you, enemy, n)))
    IdentityMessage you (AttackDefended enemy _ _) | applies you enemy ->
      pure $ NaturalAgility (attrs `With` Meta Nothing)
    CheckWindows windows | any ended windows -> pure $ NaturalAgility (attrs `With` Meta Nothing)
    _ -> NaturalAgility . (`With` meta) <$> runMessage msg attrs
   where
    applies you enemy = case defense meta of
      Just (defender, attacker, _) -> defender == you && attacker == enemy
      Nothing -> False
    ended (W.Window W.After (W.EnemyAttack enemy _)) = case defense meta of
      Just (_, attacker, _) -> attacker == enemy
      Nothing -> False
    ended _ = False
