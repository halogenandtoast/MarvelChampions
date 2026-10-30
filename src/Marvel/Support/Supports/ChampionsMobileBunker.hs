module Marvel.Support.Supports.ChampionsMobileBunker where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Query
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait

championsMobileBunker :: SupportCard ChampionsMobileBunker
championsMobileBunker = support ChampionsMobileBunker Cards.championsMobileBunker

newtype ChampionsMobileBunker = ChampionsMobileBunker (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ChampionsMobileBunker where
  getAbilities a = [ability a 1 HeroAction (OwnsThis <> SelfMatches HeroIdentity <> CharacterExists (IdentityCharacters $ IdentityWithTrait Champion)) ExhaustCost (runAbility a 1)]

instance RunMessage ChampionsMobileBunker where
  runMessage msg a@(ChampionsMobileBunker attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      champions <- selectList (IdentityWithTrait Champion)
      chooseOne you [targeting champion (cardSpecific attrs "offerDraw" champion) | champion <- champions]
      pure a
    CardSpecific (isTarget attrs -> True) "offerDraw" (fromJSON -> Success you) -> do
      chooseOne you
        [ labeled "$label.championsMobileBunkerDraw" do
            drawCards you 2
            discardFromHand you 2
        , labeled "$label.noResponse" (pure ())
        ]
      pure a
    _ -> ChampionsMobileBunker <$> runMessage msg attrs
