module Marvel.Encounter.IcemanNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability qualified as Ability
import Marvel.Attachment.Types
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers qualified as Ice
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers ()
import Marvel.Query ()
import Marvel.Status
import Marvel.Villain.Types qualified as V
import TestImport

settle :: GameAppT ()
settle = do
  pass
  cs <- asked
  case cs >>= headMay . filter forced . snd of
    Just c -> choose "mandatory ability" (== c) >> settle
    Nothing -> pure ()
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse, Ability.WhenRevealed]
    _ -> False

asIceman :: IdentityId -> GameAppT ()
asIceman p = changeForm p >> settle >> clearHand p

attachmentsWith :: CardCode -> GameAppT [Attrs Attachment]
attachmentsWith code = getsGame $ filter ((== code) . toCardCode) . map toAttrs . HashMap.elems . gameAttachments

spec :: Spec
spec = describe "Iceman nemesis and Sauron modular" do
  it "Hot-Headed remains in play and deals one nonattack damage for every attached Frostbite" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      _ <- revealNow p Cards.hotHeaded
      settle
      v <- villainIdNow
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      settle
      damageOf p `shouldReturn` 1
      L.runQueueT (L.capture $ Ice.attachFrostbite p $ EnemyVillainId v) >>= runNow
      settle
      damageOf p `shouldReturn` 2
      codes <- map toCardCode <$> encounterDiscardNow
      "46024" `elem` codes `shouldBe` False

  it "Hot-Headed is discarded after Bobby makes a basic recovery" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- revealNow p Cards.hotHeaded
      settle
      setDamage p 5
      choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
      choose "discard Hot-Headed" isAnyAbility
      settle
      damageOf p `shouldReturn` 1
      codes <- map toCardCode <$> encounterDiscardNow
      "46024" `elem` codes `shouldBe` True

  it "Pyro's quickstrike deals indirect attack damage" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      _ <- revealNow p Cards.pyroIceman
      choose "no defender" isNoDefender
      settle
      -- With no allies, the whole indirect amount belongs to Iceman.
      damageOf p `shouldReturn` 3

  it "Playing with Fire counts printed resource icons rather than discarded cards" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      scheme <- sideSchemeOf <$> revealNow p Cards.playingWithFire
      settle
      _ <- stackDeck p ["01088","01089","46009"]
      runNow [SideSchemeMessage scheme (SideSchemeThwarted (IdentityRef p) 3)]
      settle
      damageOf p `shouldReturn` 5

  it "Pyro's Flamethrower attaches to Pyro and boosts his attack by discarded icons" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      pyro <- minionOf <$> revealNow p Cards.pyroIceman
      settle
      _ <- revealNow p Cards.pyrosFlamethrower
      settle
      attached <- attachmentsWith "46027"
      map attachmentEnemy attached `shouldBe` [Just $ EnemyMinionId pyro]
      asIceman p
      _ <- stackDeck p ["01088"]
      runNow [MinionMessage pyro (MinionAttacks p)]
      settle
      damageOf p `shouldReturn` 5

  it "Pyro's Flamethrower surges when Pyro is absent" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      before <- length <$> dealtTo p
      _ <- revealNow p Cards.pyrosFlamethrower
      settle
      attachmentsWith "46027" `shouldReturn` []
      length <$> dealtTo p `shouldReturn` before + 1

  it "Burn discards two cards, or three with Pyro, and deals their printed icon count" do
    for_ [(False,3),(True,4)] $ \(withPyro,n) -> (do
      tg <- liftIO $ testGame ["iceman"]
      liftIO $ inGame tg do
        let p = player1 tg
        clearHand p
        when withPyro $ do
          _ <- revealNow p Cards.pyroIceman
          settle
        _ <- stackDeck p ["01088","46009","46011"]
        _ <- revealNow p Cards.burnIceman
        settle
        damageOf p `shouldReturn` n
      ) :: IO ()

  it "Sauron searches for Life Drain and attaches it to the highest printed-health minion" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- setEncounterDeck [Cards.lifeDrain, Cards.theEyeOfSauron] []
      sauron <- minionOf <$> revealNow p Cards.sauron
      settle
      attached <- attachmentsWith "46031"
      map attachmentEnemy attached `shouldBe` [Just $ EnemyMinionId sauron]

  it "Sauron Lives finds Sauron as a facedown encounter card" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- setEncounterDeck [Cards.sauron, Cards.theEyeOfSauron] []
      scheme <- sideSchemeOf <$> revealNow p Cards.sauronLives
      settle
      runNow [SideSchemeMessage scheme (SideSchemeThwarted (IdentityRef p) 3)]
      settle
      cards <- dealtTo p
      map toCardCode cards `shouldSatisfy` elem "46029"

  it "Life Drain deals two before its attached minion attacks and gives that minion tough" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      minion <- addMinion p Minions.armoredGuard
      _ <- revealNow p Cards.lifeDrain
      settle
      asIceman p
      runNow [MinionMessage minion (MinionAttacks p)]
      choose "Life Drain interrupt" isAnyAbility
      damageOf p `shouldReturn` 2
      settle
      characterHasCondition ToughStatus (MinionCharacter minion) `shouldReturn` True

  it "Eye of Sauron resolves each printed energy, mental and physical icon" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      belt <- putInHand p "46004"
      _ <- stackDeck p ["01088","46011"]
      before <- mainSchemeThreatNow
      _ <- revealNow p Cards.theEyeOfSauron
      settle
      choose "first energy icon" (isLabel "$label.energy")
      choose "second energy icon" (isLabel "$label.energy")
      choose "mental icon" (isLabel "$label.mental")
      choose "discard the Power Belt" (isTargetChoice belt)
      settle
      -- Energy has two icons and the mental card requires one discard.
      mainSchemeThreatNow `shouldReturn` before + 2
      handOf p `shouldReturn` []

  it "Sauron's boost heals the activating enemy and gives it tough" do
    tg <- testGame ["iceman"]
    inGame tg do
      v <- villainIdNow
      resolve [VillainMessage v (VillainDamaged GameRef $ toDamage 5 FromAbility)]
      boost <- encounterCard Cards.sauron
      runNow [RevealBoostCard boost $ EnemyVillainId v]
      settle
      villainDamageNow `shouldReturn` 2
      characterHasCondition ToughStatus (VillainCharacter v) `shouldReturn` True

  it "Burn's boost discards one player card and adds one boost icon per printed resource" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      top <- stackDeck p ["01088"]
      v <- villainIdNow
      boost <- encounterCard Cards.burnIceman
      runNow [RevealBoostCard boost $ EnemyVillainId v]
      settle
      cards <- discardOf p
      testHead top `elem` cards `shouldBe` True
      V.villainBoost . toAttrs <$> getVillain `shouldReturn` 2
      damageOf p `shouldReturn` 0

  it "Life Drain surges when stun replaces its immediate attack" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      asIceman p
      minion <- addMinion p Minions.armoredGuard
      resolve [MinionMessage minion (MinionStunned GameRef)]
      before <- length <$> dealtTo p
      _ <- revealNow p Cards.lifeDrain
      settle
      length <$> dealtTo p `shouldReturn` before + 1
      damageOf p `shouldReturn` 0
      characterHasCondition StunnedStatus (MinionCharacter minion) `shouldReturn` False

  it "Eye of Sauron applies physical damage and lets its controller choose a character to exhaust" do
    tg <- testGame ["iceman"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- stackDeck p ["46003","46023"]
      _ <- revealNow p Cards.theEyeOfSauron
      settle
      choose "physical icon" (isLabel "$label.physical")
      choose "wild icon" (isLabel "$label.wild")
      choose "exhaust Bobby" (isTargetChoice p)
      settle
      damageOf p `shouldReturn` 1
      exhaustedNow p `shouldReturn` True
