module Marvel.JsonCompatibilitySpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.Types (Parser, parseEither)
import Data.UUID qualified as UUID
import Marvel.Cost.Types qualified as Cost
import Marvel.Game.Base qualified as Game
import Marvel.Message.Identity qualified as Identity
import Marvel.Modifier.Types qualified as Modifier
import Marvel.Obligation.Obligations.JessicaLuke qualified as JL
import Marvel.Obligation.Types qualified as Obligation
import Marvel.Phase (Phase (..))
import Marvel.Window.Types qualified as W
import TestImport

-- Compare the generated codecs against the previous Generic codecs, in both
-- directions. Encoding is also decoded to exercise TH's direct toEncoding path.
compatible :: (Eq a, Show a, ToJSON a, FromJSON a) => (a -> Value) -> (Value -> Parser a) -> a -> Expectation
compatible previousEncode previousDecode value = do
  toJSON value `shouldBe` previousEncode value
  fromJSON (previousEncode value) `shouldBe` Success value
  parseEither previousDecode (toJSON value) `shouldBe` Right value
  eitherDecode (encode value) `shouldBe` Right value

spec :: Spec
spec = do
  describe "Jessica and Luke obligation saves" do
    for_ [JL.jl61030, JL.jl61033a, JL.jl61033b, JL.jl61033c, JL.jl62028] $ \builder ->
      it ("loads legacy arrays and round-trips object saves for " <> show (toCardCode builder)) do
        let JL.JLObligation attrs _ _ = buildCard builder (ObligationId UUID.nil)
            value = Obligation.Obligation $ JL.JLObligation attrs 2 True
            legacy = toJSON [toJSON attrs, toJSON (2 :: Natural), Bool True]
        fromJSON legacy `shouldBe` Success value
        eitherDecode (encode value) `shouldBe` Right value
        case toJSON value of
          Object _ -> pure ()
          _ -> expectationFailure "obligation should save as an attrs object"

    it "loads a game with a legacy obligation saved as a boost entity" do
      tg <- testGame ["jessica-jones"]
      game <- inGame tg getGame
      let oid = ObligationId UUID.nil
          JL.JLObligation attrs _ _ = buildCard JL.jl61030 oid
          obligation = Obligation.Obligation $ JL.JLObligation attrs 0 False
          saved = game {gameBoostEntities = Game.defaultEntities {Game.entitiesObligations = HashMap.singleton oid obligation}}
          legacy = toJSON [toJSON attrs, toJSON (0 :: Natural), Bool False]
          oldBoosts = case toJSON (gameBoostEntities saved) of
            Object fields -> Object $ KeyMap.insert "entitiesObligations" (toJSON $ HashMap.singleton oid legacy) fields
            value -> value
          oldSave = case toJSON saved of
            Object fields -> Object $ KeyMap.insert "boostEntities" oldBoosts fields
            value -> value
      fromJSON oldSave `shouldBe` Success saved

  describe "generated JSON compatibility" do
    let ident = IdentityId UUID.nil
        cid = CardId UUID.nil
        check = compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions)
    it "preserves every outcome, defense priority, and replacement tag" do
      mapM_
        (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
        [Won, Lost, RegistrationWon, ResistanceWon, CompetitiveDraw, BothTeamsLost]
      mapM_
        (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
        [AnyDefense, AllyIfAble]
      mapM_
        (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
        [Swapped, Advanced]
    it "preserves modifier tags, contents, nested costs, and physical card IDs" do
      mapM_
        (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
        [ Modifier.SkipNextVillainPhase
        , Modifier.BaseAttack 3
        , Modifier.ThreatRemovableOnlyByIdentity ident
        , Modifier.AttackAdditionalCostPaid GameRef
        , Modifier.AllyBasicPowerAdditionalCost (Cost.Costs [Cost.NoCost, Cost.DamageCost 2])
        , Modifier.ResourceCostReductionForCard cid 2
        , Modifier.UniqueRuleDoesNotApply "A \"quoted\" title / 日本語"
        ]
    it "preserves messages with optional values, arbitrary payloads, and recursive groups" do
      mapM_
        check
        [ StartGame
        , BeginPhase PlayerPhase
        , IdentityMessage ident (Identity.DrawCards FromDeck 2)
        , SetPlayerOrder [ident]
        , DiscardTopOfEncounterDeck 2 Nothing
        , DiscardTopOfEncounterDeck 2 (Just GameRef)
        , CardSpecific GameRef "payload" (object ["nested" .= [Null, Bool True], "text" .= ("日本語" :: Text)])
        , WithSimultaneousAttackContext 1 (Simultaneously [BeginRound, EndRound])
        , PrepareEnemyDamageGroup [StartScenario, EndRound] 1
        , ResolveEnemyDamageGroup [BeginRound] 0 [EndRound]
        ]
    it "preserves all timing, reveal-source, and threat-source variants" do
      mapM_ (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions)) [W.After, W.When, W.Would]
      mapM_
        (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
        [ W.RevealedFromEncounterDeck
        , W.RevealedFromVillain
        , W.RevealedFromSeparateDeck "test"
        , W.RevealedFromDiscardPile
        , W.RevealedFromSetAside
        , W.RevealedFromRemovedFromGame
        ]
      mapM_
        (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
        [W.ThreatFromVillain, W.ThreatFromMinion, W.ThreatFromAbility, W.ThreatFromAcceleration, W.AnyThreatSource]
    it "preserves matcher sums, window-type sums, and window record fields" do
      mapM_
        (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
        [W.CharacterAttackedInPlayArea ident, W.ExcessDamageInPlayArea ident True, W.CardWithIdWouldBeDiscardedFromPlay cid]
      let windows = [W.TurnEnded ident, W.InfinityStoneDeckRanOut, W.ChoseTrapOption ident "choice"]
      mapM_ (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions)) windows
      mapM_
        (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
        [W.Window timing window | timing <- [W.After, W.When, W.Would], window <- windows]
    it "preserves state and attack-context record codecs after moving their types" do
      mapM_
        (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
        [Game.Unstarted, Game.InProgress, Game.Finished Won]
      compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions) Game.defaultEntities
      compatible
        (genericToJSON defaultOptions)
        (genericParseJSON defaultOptions)
        (Game.TrackedEnemyAttack cid (EnemyVillainId $ VillainId UUID.nil) GameRef Nothing Nothing)
      compatible
        (genericToJSON defaultOptions)
        (genericParseJSON defaultOptions)
        (Game.SimultaneousMinionAttackGroup (MinionId UUID.nil) cid [] 0 [] Nothing)
      compatible
        (genericToJSON defaultOptions)
        (genericParseJSON defaultOptions)
        (Game.FriendlyBoostContext (AllyId UUID.nil) True [] [] 2 [])
    it "preserves live snapshots and the parser's legacy field defaults" do
      tg <- testGame ["spider-man"]
      game <- inGame tg getGame
      compatible
        (genericToJSON $ aesonOptions $ Just "game")
        (genericParseJSON $ aesonOptions $ Just "game")
        game
      case toJSON game of
        Object fields -> fromJSON (Object $ KeyMap.delete "gainedTraits" fields) `shouldBe` Success game
        _ -> expectationFailure "game did not encode as an object"
