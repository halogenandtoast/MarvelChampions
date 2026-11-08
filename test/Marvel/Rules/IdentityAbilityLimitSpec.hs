module Marvel.Rules.IdentityAbilityLimitSpec (spec) where

import TestImport
import Marvel.Ability (HasAbilities (getAbilities), passesUseLimit)
import Marvel.Ability.Types (Limit (..), baseLimit)
import Marvel.Hero.Types (Hero (..))
import Marvel.AlterEgo.Types (AlterEgo (..))

faceAbilities :: PlayerIdentitySide -> (CardCode, [Ability])
faceAbilities = \case
  HeroSide face@(Hero hero) -> (toCardCode face, getAbilities hero)
  AlterEgoSide face@(AlterEgo alterEgo) -> (toCardCode face, getAbilities alterEgo)

spec :: Spec
spec = describe "identity ability limits across forms" do
  for_ starterDecks \(name, _) ->
    it ("keeps independent printed abilities separate for " <> show name) do
      tg <- testGame [name]
      inGame tg do
        let p = player1 tg
        identity <- getPlayer p
        let faces = map (faceAbilities . snd) $ toList $ playerIdentitySides $ toAttrs identity
            collisions = [(code, a, otherCode, b)
              | (code, as) <- faces, (otherCode, bs) <- faces, code /= otherCode
              , a <- as, b <- bs, a == b, baseLimit (abilityLimit a) /= NoLimit
              ]
        for_ collisions \(code, a, otherCode, b) -> do
          unless (passesUseLimit p (fromList [(p, [b])]) a) $
            liftIO $ expectationFailure $ show (code, abilityIndex a, otherCode, abilityType a, abilityType b)
          let uses = case baseLimit (abilityLimit a) of
                PerTurn n -> n
                PerRound n -> n
                PerWindow n -> n
                PerPhase n -> n
                SharedPerWindow _ n -> n
                _ -> 0
          passesUseLimit p (fromList [(p, replicate (fromIntegral uses) a)]) a `shouldBe` False
