module Marvel.Treachery.Treacheries.NextEvolutionNextEvolutionCampaign where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Projection
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NextEvolutionCampaignTreachery = NextEvolutionCampaignTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40203 :: TreacheryCard NextEvolutionCampaignTreachery
ne40203 = treachery NextEvolutionCampaignTreachery Cards.ne40203

instance RunMessage NextEvolutionCampaignTreachery where
  runMessage msg e@(NextEvolutionCampaignTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> e <$ overburdened you a
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      overburdened you a
      pure e
    _ -> NextEvolutionCampaignTreachery <$> runMessage msg a

overburdened :: (ReverseQueue m, IsRef source) => IdentityId -> source -> m ()
overburdened you source = do
  hand <- filter ((== ResourceType) . cdCardType . getCardDef) <$> projectMap PlayerIdentityHand unHand you
  chooseOne you $
    [targeting card (discardCard card) | card <- hand]
      <> [labeled "$label.neTakeTwoDamage" (damageIdentity you source $ toDamage 2 FromAbility)]
