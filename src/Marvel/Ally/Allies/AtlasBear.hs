module Marvel.Ally.Allies.AtlasBear where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino ()
import Marvel.Deck
import Marvel.Discard ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

atlasBear :: AllyCard AtlasBear
atlasBear = ally AtlasBear Cards.atlasBear (Thw 1, 1) (Atk 1, 1) (HP 3)
newtype AtlasBear = AtlasBear (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AtlasBear where
  getAbilities a = [ability a 1 Action (OwnsThis <> ExtendedCardExists (InDeckOf AnyIdentity $ BasicCardMatches AnyCard)) ExhaustCost (runAbility a 1)]

instance RunMessage AtlasBear where
  runMessage msg a@(AtlasBear attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- filterM (fmap notNull . projectMap PlayerIdentityDeck unDeck) =<< selectList AnyIdentity
      chooseOne you [targeting p (cardSpecific attrs "look" p) | p <- players]
      pure a
    CardSpecific (isTarget attrs -> True) "look" (fromJSON -> Success p) -> do
      deck <- projectMap PlayerIdentityDeck unDeck p
      for_ (headMay deck) $ \card -> do
        focusCards [PlayerCard card]
        chooseOne (allyController attrs) $
          [labeled "$label.dominoTakeEvent" (cardSpecific attrs "takeEvent" (p,card)) | cardMatch (CardWithType EventType) card]
          <> [labeled "$label.done" (pure ())]
        unfocusCards
      pure a
    CardSpecific (isTarget attrs -> True) "takeEvent" (fromJSON -> Success (p,card)) -> do
      damageCharacter (AllyCharacter $ allyId attrs) attrs (toDamage 1 FromAbility)
      addToHand p card
      pure a
    _ -> AtlasBear <$> runMessage msg attrs
