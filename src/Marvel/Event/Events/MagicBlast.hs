module Marvel.Event.Events.MagicBlast (
  magicBlast,
  MagicBlast (..),
  printedResourceEffects,
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource

magicBlast :: EventCard MagicBlast
magicBlast = event (MagicBlast . (`With` Meta Nothing)) Cards.magicBlast

-- | The enemy attacked.
newtype Meta = Meta {enemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MagicBlast = MagicBlast (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent MagicBlast where
  toEventAttrs (MagicBlast (attrs `With` _)) = attrs

instance HasModifiersFor MagicBlast

instance IsRef MagicBlast where
  toRef = toRef . toEventAttrs

-- | What a discarded card's printed resources do, as Magic Blast and Baron Mordo read
-- them: [physical] the first, [energy] the second, [mental] the third, [wild] all three.
printedResourceEffects :: [Resource] -> a -> a -> a -> [a]
printedResourceEffects resources physical energy mental =
  [physical | has Physical] <> [energy | has Energy] <> [mental | has Mental]
 where
  has r = r `elem` resources || Wild `elem` resources

-- | Deal 5 damage to an enemy and discard the top card of your deck, which may stun the
-- enemy, deal it 2 more damage or confuse it.
instance RunMessage MagicBlast where
  runMessage msg e@(MagicBlast (attrs `With` meta)) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      pushChoice identityId $ ChooseEnemy AttackableEnemy (toTarget attrs)
      pure e
    ChoseEnemy enemyId (isTarget attrs -> True) -> do
      let ident = eventController attrs
      damage <- choiceMessages ident $ DamageEnemy (toRef enemyId) (toSource attrs) (toDamage 5 $ FromPlayerAttack ident)
      pushAll $ damage <> [IdentityMessage ident $ DiscardFrom FromDeck 1 (Just $ toTarget attrs)]
      pure . MagicBlast $ attrs `With` Meta (Just enemyId)
    WithDiscarded (isTarget attrs -> True) FromDeck (onlyPlayerCards -> cards) | Just enemyId <- enemy meta -> do
      let
        ident = eventController attrs
        target = toRef enemyId
      effects <-
        concatMapM (choiceMessages ident) $
          printedResourceEffects
            (concatMap (printedResources . getCardDef) cards)
            (Stun target (toSource attrs))
            -- damage of the same attack (RR "Attack (Player Ability Type)")
            (DamageEnemy target (toSource attrs) (toDamage 2 $ FromPlayerAttack ident))
            (Confuse target (toSource attrs))
      pushAll effects
      pure e
    _ -> MagicBlast . (`With` meta) <$> runMessage msg attrs
