module Marvel.Villain.Villains.Drang where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Environment.Environments.BadoonShip (chargeUp)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W

-- | Drang (The Galaxy's Most Wanted, Brotherhood of Badoon): I and II in standard, II and
-- III in expert.
newtype Drang = Drang (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

drang :: VillainCard Drang
drang = drang1

drang1 :: VillainCard Drang
drang1 = villain Drang Cards.drang1 (Sch 1) (Atk 2) (HP $ PerPlayer 13)

drang2 :: VillainCard Drang
drang2 = villainWith Drang Cards.drang2 (Sch 2) (Atk 3) (HP $ PerPlayer 14) (stageL .~ 2)

drang3 :: VillainCard Drang
drang3 = villainWith Drang Cards.drang3 (Sch 3) (Atk 3) (HP $ PerPlayer 18) (stageL .~ 3)

-- | I and II: [star] Forced Response: after Drang schemes, Charge Up. III: after he
-- activates (schemes, or attacks), Charge Up. II and III have a When Revealed.
instance HasAbilities Drang where
  getAbilities (Drang a) =
    [ windowAbility a 1 (VillainRevealed When (VillainWithId vid) RevealedFromVillain) WhenRevealed NoCost $
        RunAbility (toTarget a) 1
    | villainStage a >= 2
    ]
      <> [ windowAbility a 2 activated ForcedResponse NoCost $ RunAbility (toTarget a) 2
         ]
   where
    vid = villainId a
    schemed = W.VillainSchemed After (VillainWithId vid)
    activated
      | villainStage a >= 3 = WindowOneOf [schemed, EnemyAttacked After (EnemyWithId $ EnemyVillainId vid) AnyIdentity]
      | otherwise = schemed

instance RunMessage Drang where
  runMessage msg e@(Drang attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (Normal, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo drang2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo drang3 attrs
        _ -> Drang <$> runMessage msg attrs
    -- II: with Drang's Spear in play, Drang is given a facedown boost card; otherwise it
    -- is found in the encounter deck or discard pile and revealed, the deck shuffled.
    -- III: the top 4[per player] encounter cards discarded one by one, each minion put
    -- into play with the player engaged with the fewest minions as it is discarded,
    -- before the next is (it may engage and deal a card from the deck first). The deck
    -- emptied by the discard, it is fulfilled (RR Encounter Deck).
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      first <- selectJust FirstPlayer
      case villainStage attrs of
        2 -> do
          spear <- selectAny (AttachmentIs Attachments.drangsSpear)
          if spear
            then dealBoost attrs
            else searchAndRevealEncounterCard first (CardIs Attachments.drangsSpear)
        _ -> do
          n <- fromGameValue (PerPlayer 4)
          cardSpecific attrs "discardNext" (n :: Int)
      pure e
    CardSpecific (isTarget attrs -> True) "discardNext" (fromJSON -> Success (n :: Int)) | n > 0 -> do
      deck <- getEncounterDeck
      discardTopOfEncounterDeckThen 1 \cards ->
        for_ [c | c <- cards, cdCardType (getCardDef c) == MinionType] \c ->
          cardSpecific attrs "engageFewest" c
      when (length deck > 1) $ cardSpecific attrs "discardNext" (n - 1)
      pure e
    CardSpecific (isTarget attrs -> True) "engageFewest" (fromJSON -> Success card) -> do
      players <- getPlayers
      scored <- for players \p -> (p,) <$> selectListCount (MinionEngagedWith $ IdentityWithId p)
      chooseAmongTied (extremes LeastThreat scored) \p -> putEncounterCardIntoPlay p card
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> e <$ chargeUp
    _ -> Drang <$> runMessage msg attrs
