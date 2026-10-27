module Marvel.Hero.DraxSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGame)
import TestImport

draxGame :: IO TestGame
draxGame = testGame ["drax"]

-- | Drax in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | Drax's own ability: the hero's vengeance response, the alter-ego's forced response.
isOwn :: IdentityId -> Choice -> Bool
isOwn p = \case
  UseAbility ab -> runAbility p 1 `elem` abilityChoices ab
  _ -> False

countersOf :: IdentityId -> GameAppT Natural
countersOf p = (.playerIdentityCounters) <$> playerAttrs p

placeVengeance :: IdentityId -> Natural -> GameAppT ()
placeVengeance p n = runNow [PlaceCounters (toTarget p) n]

-- | The villain attacks Drax, who declares no defender.
villainAttacksUndefended :: IdentityId -> GameAppT ()
villainAttacksUndefended p = do
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]
  choose "no defender" isNoDefender

-- | A number the board shows for the player.
shown :: Key -> GameAppT [Natural]
shown field = do
  view <- liftIO . publicGame =<< getGame
  pure
    [ floor n
    | Object o <- [view]
    , Just (Array ps) <- [KeyMap.lookup "players" o]
    , Object pl <- toList ps
    , Just (Number n) <- [KeyMap.lookup field pl]
    ]

isMantisHeal :: Choice -> Bool
isMantisHeal = \case
  UseAbility ab -> any (\case ChooseHeal 3 _ -> True; _ -> False) (abilityChoices ab)
  _ -> False

spec :: Spec
spec = describe "Drax" do
  describe "Drax (19001a)" do
    it "after the villain attacks him undefended, places a vengeance counter, +1 ATK each, shown on the board" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        asHero p
        shown "attack" `shouldReturn` [1]
        villainAttacksUndefended p
        choose "place vengeance" (isOwn p)
        countersOf p `shouldReturn` 1
        shown "counters" `shouldReturn` [1]
        shown "attack" `shouldReturn` [2]

    it "places a counter when Drax defends himself" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        asHero p
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "Drax defends" isDefend
        choose "place vengeance" (isOwn p)
        countersOf p `shouldReturn` 1

    it "places none when an ally defends" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        asHero p
        mantis <- putIntoPlay p "19002"
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "Mantis defends" \case AllyDefend aid _ -> aid == allyIdOf mantis; _ -> False
        offers (isOwn p) `shouldReturn` False
        pass
        countersOf p `shouldReturn` 0

    it "at 3 counters, draws 1 card instead" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeVengeance p 3
        shown "attack" `shouldReturn` [4]
        villainAttacksUndefended p
        choose "place vengeance" (isOwn p)
        countersOf p `shouldReturn` 3
        length <$> handOf p `shouldReturn` 1

    it "keeps his counters as he changes form to hero" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        placeVengeance p 2
        asHero p
        countersOf p `shouldReturn` 2
        shown "attack" `shouldReturn` [3]

  describe "Drax (19001b)" do
    it "after changing to this form, removes all vengeance counters and heals 2 for each" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeVengeance p 2
        setDamage p 6
        runNow [IdentityMessage p (ChangedToForm B)]
        chooseIf (isOwn p)
        countersOf p `shouldReturn` 0
        damageOf p `shouldReturn` 2

  describe "Mantis (19002)" do
    it "exhausts and takes 1 damage to heal 3 damage from an identity" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "19002"
        setDamage p 5
        choose "use Mantis" isMantisHeal
        damageOf p `shouldReturn` 2
        mantis <- allyIn "19002"
        mantis.allyDamage `shouldBe` 1
        mantis.allyExhausted `shouldBe` True
        offers isMantisHeal `shouldReturn` False

  describe "Drax's Knife (19008)" do
    it "gives Drax +1 ATK in hero form" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "19008"
        shown "attack" `shouldReturn` [2]

  describe "Drax's Other Knife (19009)" do
    it "gives Drax retaliate 1 in hero form" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "19009"
        damage <- villainDamageNow
        villainAttacksUndefended p
        pass
        villainDamageNow `shouldReturn` (damage + 1)

    it "gives no retaliate in alter-ego form" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "19009"
        damage <- villainDamageNow
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        pass
        villainDamageNow `shouldReturn` damage

  describe "DWI Theet Mastery (19010)" do
    it "after Drax makes a basic attack, draws 1 card" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        asHero p
        dwi <- putIntoPlay p "19010"
        choose "attack" (isHeroAttack p)
        villainIdNow >>= chooseIf . isTargetChoice
        choose "use DWI Theet Mastery" (isAbilityOfCard dwi)
        length <$> handOf p `shouldReturn` 1

  describe "Too Stubborn to Die (19011)" do
    it "instead of Drax being defeated, sets his dial to 4, changes him to alter-ego and is removed from the game" do
      tg <- draxGame
      let p = player1 tg
      inGame tg do
        asHero p
        stubborn <- putIntoPlay p "19011"
        placeVengeance p 2
        setDamage p 12
        runNow (damageIdentity p GameRef (toDamage 5 FromAbility))
        choose "use Too Stubborn to Die" (isAbilityOfCard stubborn)
        chooseIf (isOwn p)
        a <- playerAttrs p
        a.playerIdentitySide `shouldBe` B
        a.playerIdentityDefeated `shouldBe` False
        -- 4 hit points left, then 2 healed for each of the 2 counters removed
        damageOf p `shouldReturn` 6
        countersOf p `shouldReturn` 0
        upgradesOf p `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` notElem stubborn)
