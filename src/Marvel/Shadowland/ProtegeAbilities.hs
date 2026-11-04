module Marvel.Shadowland.ProtegeAbilities where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attack
import Marvel.Card
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

protegeAbilities :: IsRef source => source -> CardCode -> EnemyId -> [Ability]
protegeAbilities source code enemyId = case code of
  c | c `elem` ["65012a","65012b"] -> [afterAttack 1]
  c | c `elem` ["65013a","65013b"] -> [limited (EachPlayer $ PerRound 1) $ windowAbility source 2 (W.WindowOneOf [W.IdentityAttacked W.After AnyIdentity enemy,W.AllyAttacked W.After AnyAlly enemy]) ForcedResponse NoCost (runAbility source 2)]
  c | c `elem` ["65014a","65014b"] -> [beforeAttack 3]
  c | c `elem` ["65015a","65015b"] -> [beforeAttack 4]
  c | c `elem` ["65016a","65016b"] -> [afterAttack 5]
  c | c `elem` ["65017a","65017b"] -> [limited (EachPlayer $ PerRound 1) $ afterAttack 6]
  _ -> []
 where
  enemy = EnemyWithId enemyId
  afterAttack index = windowAbility source index (W.EnemyAttacked W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility source index)
  beforeAttack index = windowAbility source index (W.EnemyWouldAttack enemy AnyIdentity) ForcedInterrupt NoCost (runAbility source index)

resolveProtegeAbility :: (ReverseQueue m, IsRef source) => source -> CardCode -> EnemyId -> Maybe Attack -> IdentityId -> Natural -> [W.WindowType] -> m ()
resolveProtegeAbility source code enemy attack you index windows = do
  allyControllers <- traverse (getSourceController . AllyRef) [aid | W.AllyAttack aid _ <- windows]
  let who = fromMaybe you $ headMay ([p | W.EnemyAttack _ p <- windows] <> [p | W.EnemyAttackAlly _ _ p <- windows] <> [p | W.PlayerAttackResolved _ p _ <- windows] <> [p | W.IdentityAttack p _ <- windows] <> [p | Just p <- allyControllers])
  case index of
    1 -> do
      minions <- selectList AnyMinion
      eligible <- filterM (\mid -> characterMatches CharacterCanGainTough (MinionCharacter mid)) minions
      if null eligible then discardTopOfEncounterDeckThen 3 $ \cards -> chooseOne who [cardLabeled (toCardCode c) (dealFoundEncounterCard who c) | c <- filter (cardMatch $ CardWithType MinionType) cards]
        else chooseOne who [targeting mid (giveStatus mid ToughStatus) | mid <- eligible]
    2 -> do
      payable <- canAffordCost who (toSource source) (DiscardRandomHandCardsCost 1)
      chooseOne who $ [labeled "$label.slDiscardRandom" (discardFrom who RandomFromHand 1) | payable] <> [labeled "$label.slProtegeAttacks" (enemyAttacks enemy who)]
    3 -> discardFromDeck who 1 source
    4 -> do
      payable <- canAffordCost who (toSource source) (ResourceCost $ Just Physical)
      canStun <- characterCanGainStatus (IdentityCharacter who) StunnedStatus
      chooseOne who $ [labeled "$label.slSpendPhysical" (payForTreachery who $ ResourceCost $ Just Physical) | payable]
        <> [labeled "$label.stunYourIdentity" (stunIdentity who) | canStun]
        <> [labeled "$label.slTwoIndirect" (dealIndirectDamage who source 2)]
    5 -> if maybe False attackDefended attack then selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn source mid 1) else dealIndirectDamage who source 1
    6 -> do
      let bonus = if code == "65017a" then 1 else 0
      case enemy of
        EnemyVillainId vid -> villainAttacksCharacterWithBonus vid who who bonus []
        EnemyMinionId mid -> minionAttacksWithChange mid who bonus []
        EnemyAttachmentId _ -> pure ()
    _ -> pure ()

resolveElektraDiscard :: ReverseQueue m => EnemyId -> [Card] -> m ()
resolveElektraDiscard enemy cards = do
  let oddCost = any (maybe False odd . cdCost . getCardDef) cards
  if oddCost then changeAttack enemy 1 else attackGains enemy Overkill >> attackGains enemy Piercing
