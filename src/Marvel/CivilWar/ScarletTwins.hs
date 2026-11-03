module Marvel.CivilWar.ScarletTwins where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.Cost
import Marvel.Damage
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.Obligation.Types
import Marvel.SideScheme.Types
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype ScarletTwin = ScarletTwin (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

scarletTwins :: [MinionCard ScarletTwin]
scarletTwins = map make [C.cw57060,C.cw57061]
 where
  make def = let (sch,atk,hp) = C.printedMinionStats (toCardCode def)
    in minion ScarletTwin def (Sch sch) (Atk atk) (HP hp)

instance RunMessage ScarletTwin where
  runMessage msg e@(ScarletTwin a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      let status = if toCardCode a == "57060" then StunnedStatus else ConfusedStatus
      before <- getCharacterStatusCount (IdentityCharacter you) status
      giveStatus (IdentityCharacter you) status
      cardSpecific a "afterStatus" (you,status,before)
      pure e
    CardSpecific (isTarget a -> True) "afterStatus" (fromJSON -> Success (you,status,before)) -> do
      after <- getCharacterStatusCount (IdentityCharacter you) status
      when (after <= (before :: Natural)) $
        if toCardCode a == "57060" then damageIdentity you a (toDamage 2 FromAbility)
        else do
          schemes <- asYou you $ selectList EncounterMainScheme
          traverse_ (\scheme -> placeThreatOn a scheme 2) schemes
      pure e
    _ -> ScarletTwin <$> runMessage msg a

newtype Superspeed = Superspeed (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

superspeed :: TreacheryCard Superspeed
superspeed = treachery Superspeed C.cw57062

instance RunMessage Superspeed where
  runMessage msg e@(Superspeed a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      discardFor you a RandomFromHand 1 1
      pure $ Superspeed a {treacheryResolver = Just you}
    WithDiscarded (isTarget a -> True) _ cards -> do
      let n = fromIntegral $ sum [max 0 $ fromMaybe 0 $ cdCost $ getCardDef card | card <- cards]
      for_ (treacheryResolver a) $ \you -> when (n > 0) $ dealIndirectDamage you a n
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      discardFor you a FromHand 1 1
      pure e
    _ -> Superspeed <$> runMessage msg a

newtype Spellcasting = Spellcasting (Attrs Obligation)
  deriving anyclass (IsObligation,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

spellcasting :: ObligationCard Spellcasting
spellcasting = obligation Spellcasting C.cw57063

instance HasAbilities Spellcasting where
  getAbilities (Spellcasting a) =
    [windowAbility a 1 (W.PlaysCard W.When (IdentityWithId you) AnyCard) ForcedInterrupt NoCost (runAbility a 1)
    | Just you <- [obligationOwner a]]

instance RunMessage Spellcasting where
  runMessage msg e@(Spellcasting a) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a -> pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [(you,card) | W.CardPlayed you card <- windows,Just you == obligationOwner a] $ \(you,card) -> do
        canceled <- cancelPlayerCardEffects you card
        when canceled $ removeFromPlay a
      pure e
    _ -> Spellcasting <$> runMessage msg a

newtype SuperpoweredSiblings = SuperpoweredSiblings (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

superpoweredSiblings :: SideSchemeCard SuperpoweredSiblings
superpoweredSiblings = sideScheme SuperpoweredSiblings C.cw57064 (Static 3)

instance RunMessage SuperpoweredSiblings where
  runMessage msg (SuperpoweredSiblings a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      you <- getDefeatedBy a
      discardFor you a FromHand 1 1
      SuperpoweredSiblings <$> runMessage msg a
    _ -> SuperpoweredSiblings <$> runMessage msg a
