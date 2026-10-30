module Marvel.Ally.Allies.ScarletSpider where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

import Marvel.Damage

scarletSpider :: AllyCard ScarletSpider
scarletSpider = ally ScarletSpider Cards.scarletSpider (Thw 1, 1) (Atk 2, 1) (HP 4)

newtype ScarletSpider = ScarletSpider (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ScarletSpider where
  getAbilities a@(ScarletSpider attrs) =
    [limitedWindowAbility a 1 (W.EncounterCardRevealedBy W.Would (IdentityWithId $ allyController attrs) AnyEncounterCard)
      Interrupt OwnsThis NoCost (runAbility a 1)]

instance RunMessage ScarletSpider where
  runMessage msg a@(ScarletSpider attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.EncounterCardRevealed _ who card <- windows, who == you] $ \card ->
        chooseOne you [labeled (typeLabel typ) (cardSpecific attrs "named" (typ,card)) | typ <- nameable]
      pure a
    CardSpecific (isTarget attrs -> True) "named" (fromJSON -> Success (typ,card)) -> do
      lookingAt (allyController attrs) [EncounterCard card] $ chooseOne (allyController attrs)
        [labeled "$label.done" do
          when (cdCardType (getCardDef card) == typ) do
            damageCharacter (allyId attrs) attrs (toDamage 1 FromAbility)
            drawCards (allyController attrs) 1]
      pure a
    _ -> ScarletSpider <$> runMessage msg attrs

-- Naming precedes looking, so the choice itself exposes no encounter-card information.
nameable :: [CardType]
nameable = [AllyType, EventType, ResourceType, SupportType, UpgradeType, AttachmentType,
  EnvironmentType, MinionType, ObligationType, SideSchemeType, TreacheryType]

typeLabel :: CardType -> Text
typeLabel typ = "$label.name" <> tshow typ
