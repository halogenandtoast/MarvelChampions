module Marvel.Upgrade.Upgrades.WideStance (wideStance, WideStance (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Count
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Heroes.NebulaHero (techniqueResolved)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

wideStance :: UpgradeCard WideStance
wideStance = upgrade WideStance Cards.wideStance

newtype WideStance = WideStance (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade WideStance where
  upgradeSpecialCanResolve (WideStance attrs) = do
    pile <- getPile =<< getDeckKeyFor (upgradeController attrs)
    pure $ not (null $ pileDeck pile)

instance HasAbilities WideStance where
  getAbilities _ = []

instance HasModifiersFor WideStance where
  getModifiersFor _ (IdentityRef ident) (WideStance attrs)
    | ident == upgradeController attrs = do
        hero <- selectAny (IdentityWithId ident <> HeroIdentity)
        pure $ if hero then [AttackDamageReduction 1] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage WideStance where
  runMessage msg u@(WideStance attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      lookAtTopOfEncounterDeck 3 attrs
      pure u
    SearchFoundCards (isTarget attrs -> True) cards -> do
      let looked = [c | EncounterCard c <- cards]
      unless (null looked) do
        lookingAt ident cards $ chooseOne ident
          [targeting card do
            discardCard card
            cardSpecific attrs "wideStanceOrder" (filter (/= card) looked, [] :: [EncounterCard])
          | card <- looked]
      pure u
    CardSpecific (isTarget attrs -> True) "wideStanceOrder" (fromJSON -> Success (remaining :: [EncounterCard], ordered :: [EncounterCard])) -> do
      case remaining of
        [] -> do
          putOnTopOfEncounterDeck ordered
          techniqueResolved ident (upgradeId attrs)
        [card] -> cardSpecific attrs "wideStanceOrder" ([] :: [EncounterCard], ordered <> [card])
        _ -> chooseOne ident
          [targeting card $ cardSpecific attrs "wideStanceOrder" (filter (/= card) remaining, ordered <> [card])
          | card <- remaining]
      pure u
    _ -> WideStance <$> runMessage msg attrs
   where ident = upgradeController attrs
