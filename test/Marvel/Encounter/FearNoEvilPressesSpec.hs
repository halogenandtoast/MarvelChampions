module Marvel.Encounter.FearNoEvilPressesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Message.Lifted qualified as L
import Marvel.Support.Types qualified as Support
import TestImport

staff :: IdentityId -> CardDef -> GameAppT SupportId
staff you def = do
  card <- encounterCard def
  resolve =<< L.runQueueT (L.capture $ L.putEncounterCardIntoPlay you card)
  pure $ SupportId $ unCardId $ ecCardId card

staffExhausted :: SupportId -> GameAppT Bool
staffExhausted sid = getsGame $ Support.supportExhausted . toAttrs . (HashMap.! sid) . gameSupports

spec :: Spec
spec = describe "Fear No Evil Daily Bugle behaviors" do
  it "Robbie Robertson looks at two encounter cards and discards the chosen one for one use" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sid <- staff you Cards.street60153
      ([first,second,_],_) <- setEncounterDeck [Cards.street60188,Cards.street60174,Cards.street60188] []
      choose "use Robbie" (isAbilityOf sid)
      choose "discard the second encounter card" (isTargetChoice second)
      pass
      getCountersOn sid `shouldReturn` 2
      staffExhausted sid `shouldReturn` True
      getEncounterDeck >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [ecCardId first]
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [ecCardId second]

  it "Betty Brant draws two cards and deals an encounter card as her additional cost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sid <- staff you Cards.street60155
      clearHand you
      _ <- setEncounterDeck [Cards.street60188,Cards.street60188] []
      choose "use Betty" (isAbilityOf sid)
      pass
      length <$> handOf you `shouldReturn` 2
      length . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` 1
      getCountersOn sid `shouldReturn` 2
      staffExhausted sid `shouldReturn` True

  it "Norah Winters can replace a freshly dealt physical encounter card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sid <- staff you Cards.street60156
      ([first,second,_],_) <- setEncounterDeck [Cards.street60174,Cards.street60188,Cards.street60188] []
      runNow [DealEncounterCard you]
      choose "use Norah's response" (isAbilityOf sid)
      choose "replace the dealt card" (isLabel "$label.fneReplaceEncounterCard")
      pass
      map ecCardId . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` [ecCardId second]
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [ecCardId first]
      getCountersOn sid `shouldReturn` 2

  it "Ben Urich cancels a flipped boost's icons and effect, then supplies a replacement boost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sid <- staff you Cards.street60154
      vid <- villainIdNow
      ([first,second,_],_) <- setEncounterDeck [Cards.street60174,Cards.street60188,Cards.street60188] []
      runNow [VillainMessage vid $ VillainSchemesAgainst you]
      choose "use Ben's interrupt" (isAbilityOf sid)
      pass
      mainSchemeThreatNow `shouldReturn` 1
      getCountersOn sid `shouldReturn` 2
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldMatchList` [ecCardId first,ecCardId second]

  it "The Daily Bugle replenishes a staff use by exhausting that staff in alter-ego form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sid <- staff you Cards.street60153
      card <- revealNow you Cards.street60152
      pass
      let eid = EnvironmentId $ unCardId $ ecCardId card
      choose "use the Daily Bugle" (isAbilityOf eid)
      pass
      getCountersOn sid `shouldReturn` 4
      staffExhausted sid `shouldReturn` True

  it "Exclusive Interview exhausts all Daily Bugle staff and stops their readying until it leaves" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      own <- staff (player1 tg) Cards.street60153
      other <- staff (player2 tg) Cards.street60154
      card <- revealNow (player1 tg) Cards.street60157
      pass
      staffExhausted own `shouldReturn` True
      staffExhausted other `shouldReturn` True
      resolve [SupportMessage own ReadiedSupport]
      staffExhausted own `shouldReturn` True
      resolve [RemoveFromPlay $ SideSchemeRef $ SideSchemeId $ unCardId $ ecCardId card,SupportMessage own ReadiedSupport]
      staffExhausted own `shouldReturn` False

  it "Breaking News can exhaust a ready staff and spend one of its uses instead of adding threat" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sid <- staff you Cards.street60156
      before <- mainSchemeThreatNow
      _ <- revealNow you Cards.street60158
      choose "exhaust Norah" (isTargetChoice sid)
      pass
      mainSchemeThreatNow `shouldReturn` before
      getCountersOn sid `shouldReturn` 2
      staffExhausted sid `shouldReturn` True
