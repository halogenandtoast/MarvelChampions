{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.AgentsOfShield.PlayerHelpers where
import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as T
import Marvel.Support.Types (Support, Field(SupportCardDef,SupportExhausted))
import Marvel.Upgrade.Types (Upgrade, Field(UpgradeCardDef))

shieldSupports :: HasGame m => IdentityId -> m [SupportId]
shieldSupports you = selectList (SupportMatches [SupportWithTrait T.Shield, SupportControlledBy (IdentityWithId you)])

chooseShieldCounter :: ReverseQueue m => IdentityId -> source -> m ()
chooseShieldCounter you _ = do
  supports <- selectList (SupportWithTrait T.Shield)
  chooseOne you [targeting sid (placeCounters sid 1) | sid <- supports]

supportCost :: HasGame m => SupportId -> m Natural
supportCost sid = fromIntegral . max 0 . fromMaybe 0 . (>>= cdCost) <$> getsGame (`abilityCardDef` SupportRef sid)

chooseBudgetSupports :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> [SupportId] -> Bool -> m ()
chooseBudgetSupports you source budget used counter = do
  supports <- selectList (SupportWithTrait T.Shield)
  eligible <- if counter then pure supports else filterM (isExhaustedSupport) supports
  options <- filterM (\sid -> (<= budget) <$> supportCost sid) (filter (`notElem` used) eligible)
  chooseOne you $ labeled "$label.done" (pure ()) :
    [targeting sid do
      cost <- supportCost sid
      if counter then placeCounters sid 1 else ready sid
      cardSpecific source "budgetSupports" (you,budget-cost,used<>[sid],counter)
    | sid <- options]

suitForm :: HasGame m => IdentityId -> m (Maybe UpgradeId)
suitForm you = selectOne (UpgradeControlledBy (IdentityWithId you) <> UpgradeOneOf [UpgradeWithCard (CardWithTitle "Assault"),UpgradeWithCard (CardWithTitle "Stealth")])

isStealth :: HasGame m => IdentityId -> m Bool
isStealth you = selectAny (UpgradeControlledBy (IdentityWithId you) <> UpgradeWithCard (CardWithTitle "Stealth"))

placeSuitThreat :: ReverseQueue m => IdentityId -> Natural -> m ()
placeSuitThreat you n = suitForm you >>= traverse_ (\uid -> cardSpecific uid "suitThreat" (True,n))

removeSuitThreat :: ReverseQueue m => IdentityId -> Natural -> m ()
removeSuitThreat you n = suitForm you >>= traverse_ (\uid -> cardSpecific uid "suitThreat" (False,n))

changeSuit :: ReverseQueue m => IdentityId -> Bool -> m ()
changeSuit you stealth = suitForm you >>= traverse_ (\uid -> cardSpecific uid "changeSuit" stealth)

suitThreat :: HasGame m => IdentityId -> m Natural
suitThreat you = suitForm you >>= maybe (pure 0) (getCountersOn . UpgradeRef)

isExhaustedSupport :: HasGame m => SupportId -> m Bool
isExhaustedSupport sid = elem sid <$> selectList (SupportWithExhaustion True)

-- The rulesheet's Informant/Stealth interaction changes only the main-scheme
-- part to removal; the single point intercepted by the suit is still collected.
informantScheme :: ReverseQueue m => IdentityId -> Source -> SchemeId -> Natural -> m ()
informantScheme you source scheme n = do
  stealth <- isStealth you
  stored <- suitThreat you
  let diverted = if stealth && stored <= 5 && n > 0 then 1 else 0
  when (diverted > 0) $ placeSuitThreat you diverted
  removeThreat you source (n-diverted) (SchemeWithId scheme)
