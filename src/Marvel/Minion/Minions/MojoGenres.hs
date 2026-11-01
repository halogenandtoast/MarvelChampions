module Marvel.Minion.Minions.MojoGenres where

import Data.List (nub)
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Projection
import Marvel.Resource
import Marvel.Window qualified as W

-- The player who defeated a fantasy minion and Magneto's growing magnetic counters.
data GenreMinionMeta = GenreMinionMeta
  { genreDefeater :: Maybe IdentityId
  , magneticCounters :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MojoGenreMinion = MojoGenreMinion (Attrs Minion `With` GenreMinionMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion MojoGenreMinion where
  toMinionAttrs (MojoGenreMinion (a `With` _)) = a
  minionDisplayCounters (MojoGenreMinion (_ `With` meta)) = magneticCounters meta
  overMinionAttrs f (MojoGenreMinion (a `With` m)) = MojoGenreMinion (f a `With` m)

mkGenreMinion :: Attrs Minion -> MojoGenreMinion
mkGenreMinion a = MojoGenreMinion (a `With` (GenreMinionMeta Nothing 0))

mojo39042, mojo39043, mojo39044, mojo39049, mojo39050, mojo39051, mojo39052, mojo39054, mojo39055, mojo39056, mojo39057, mojo39058, mojo39068, mojo39069 :: MinionCard MojoGenreMinion
mojo39042 = minion mkGenreMinion Cards.mojo39042 (Sch 1) (Atk 3) (HP 10)
mojo39043 = minion mkGenreMinion Cards.mojo39043 (Sch 2) (Atk 1) (HP 1)
mojo39044 = minion mkGenreMinion Cards.mojo39044 (Sch 0) (Atk 2) (HP 7)
mojo39049 = minion mkGenreMinion Cards.mojo39049 (Sch 1) (Atk 1) (HP 4)
mojo39050 = minion mkGenreMinion Cards.mojo39050 (Sch 0) (Atk 2) (HP 6)
mojo39051 = minion mkGenreMinion Cards.mojo39051 (Sch 2) (Atk 2) (HP 6)
mojo39052 = minion mkGenreMinion Cards.mojo39052 (Sch 1) (Atk 3) (HP 4)
mojo39054 = minion mkGenreMinion Cards.mojo39054 (Sch 1) (Atk 3) (HP 5)
mojo39055 = minion mkGenreMinion Cards.mojo39055 (Sch 1) (Atk 1) (HP 6)
mojo39056 = minion mkGenreMinion Cards.mojo39056 (Sch 1) (Atk 1) (HP 7)
mojo39057 = minion mkGenreMinion Cards.mojo39057 (Sch 2) (Atk 2) (HP 4)
mojo39058 = minion mkGenreMinion Cards.mojo39058 (Sch 2) (Atk 2) (HP 3)
mojo39068 = minion mkGenreMinion Cards.mojo39068 (Sch 1) (Atk 3) (HP 7)
mojo39069 = minion mkGenreMinion Cards.mojo39069 (Sch 1) (Atk 2) (HP 3)

instance HasModifiersFor MojoGenreMinion where
  getModifiersFor _ target (MojoGenreMinion (a `With` _))
    | target == toRef a = pure $ case toCardCode a of
        "39042" -> [DamageMultiplierFromResource Energy 2]
        "39043" -> [CannotTakeDamageExceptFromResource Physical]
        "39044" -> [DamageBonusFromResource Mental 1]
        "39051" -> [AttackDamageMultiplierWithKeyword Piercing 2]
        "39055" -> [AttackDamageCap 2]
        _ -> []
  getModifiersFor _ _ _ = pure []

instance HasAbilities MojoGenreMinion where
  getAbilities (MojoGenreMinion (a `With` _)) =
    [windowAbility a 1 (W.EnemyActivates W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility a 1) | code `elem` ["39049", "39050", "39056"]]
    <> [windowAbility a 2 (W.EngagesMinion W.After AnyIdentity $ MinionWithId mid) ForcedResponse NoCost (runAbility a 2) | code `elem` ["39054", "39058"]]
    <> [windowAbility a 3 (W.EnemyAttackedAndDamaged enemy AnyCharacter) ForcedResponse NoCost (runAbility a 3) | code == "39051"]
    <> [windowAbility a 4 (W.EnemyWouldAttack enemy AnyIdentity) ForcedInterrupt NoCost (runAbility a 4) | code == "39057"]
    <> [windowAbility a 5 (W.EnemyAttacked W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility a 5) | code == "39068"]
   where
    code = toCardCode a
    mid = minionId a
    enemy = EnemyWithId $ EnemyMinionId mid

instance RunMessage MojoGenreMinion where
  runMessage msg e@(MojoGenreMinion (attrs `With` (GenreMinionMeta defeater magnetic))) = runQueueT $ case msg of
    -- This replacement occurs before defeat windows or overkill are resolved.
    MinionMessage mid (MinionDefeatedWithExcess _ _ _) | mid == minionId attrs, toCardCode attrs == "39052", card : rest <- minionHeldCards attrs -> do
      discardCard card
      pure $ MojoGenreMinion (attrs {minionHeldCards = rest, minionDamage = 0} `With` (GenreMinionMeta defeater magnetic))
    MinionMessage mid MinionDefeated | mid == minionId attrs, toCardCode attrs == "39052", card : rest <- minionHeldCards attrs -> do
      discardCard card
      pure $ MojoGenreMinion (attrs {minionHeldCards = rest, minionDamage = 0} `With` (GenreMinionMeta defeater magnetic))
    MinionMessage mid (MinionDefeatedWithExcess source _ _) | mid == minionId attrs -> do
      player <- getSourceController source
      a <- runMessage msg attrs
      pure $ MojoGenreMinion (a `With` (GenreMinionMeta player magnetic))
    MinionMessage mid (MinionEngagedIdentity you) | mid == minionId attrs, toCardCode attrs == "39069" -> do
      -- The duel is offered before the base handler schedules quickstrike.
      canPay <- asYou you $ canAffordCost you (toSource attrs) (MultiResourceCost [Just Energy, Just Energy])
      when canPay $ chooseOne you
        [ labeled "$label.mojoDuel" do
            payForTreachery you (MultiResourceCost [Just Energy, Just Energy])
            cardSpecific attrs "duel" you
        , labeled "$label.done" (pure ())
        ]
      MojoGenreMinion . (`With` (GenreMinionMeta defeater magnetic)) <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "duel" (fromJSON -> Success you) -> do
      amount <- characterAttackValue (IdentityCharacter you)
      dealDamage (minionId attrs) attrs (toDamage amount FromAbility)
      pure e
    MinionMessage mid (RevealMinion you) | mid == minionId attrs, toCardCode attrs == "39052" -> do
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      unless (null allies) $ chooseOne you [targeting aid do
        card <- getsGame (`cardInPlay` AllyRef aid)
        for_ card $ \case
          PlayerCard c -> do
            defeatAlly aid
            cardSpecific attrs "holdDefeated" c
          _ -> pure ()
        | aid <- allies]
      MojoGenreMinion . (`With` (GenreMinionMeta defeater magnetic)) <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "holdDefeated" (fromJSON -> Success card) -> do
      attachPlayerCardToMinion (minionId attrs) card
      pure e
    MinionMessage mid MinionDefeated | mid == minionId attrs -> do
      you <- maybe getYou pure defeater
      case toCardCode attrs of
        "39042" -> getPlayers >>= traverse_ (`drawCards` 4)
        "39043" -> removeThreat you attrs 2 AnyScheme
        "39044" -> do
          cards <- filter (cardMatch $ CardWithType AllyType) . unDiscard <$> project PlayerIdentityDiscard you
          unless (null cards) $ chooseOne you
            (labeled "$label.done" (pure ()) : [targeting c (putIntoPlay you c) | c <- cards])
        "39050" -> selectList AnyCharacter >>= traverse_ (\c -> when (friendly c) $ healFrom attrs c 1)
        "39058" -> do
          for_ (minionHeldCards attrs) $ \card -> for_ (pcOwner card) (`addToHand` card)
          pure ()
        _ -> pure ()
      let current = if toCardCode attrs == "39058" then attrs {minionHeldCards = []} else attrs
      MojoGenreMinion . (`With` (GenreMinionMeta defeater magnetic)) <$> runMessage msg current
    RanAbility you (isTarget attrs -> True) 1 _ _ -> case toCardCode attrs of
      "39049" -> do
        searchAndPutIntoPlayEncounterCard you (CardIs Cards.mojo39050)
        removeFromPlay attrs
        pure e
      "39050" -> do
        characters <- selectList AnyCharacter
        for_ (filter ((/= toCharacter (minionId attrs))) characters) $ \c ->
          damageCharacter c attrs (toDamage 1 FromAbility)
        pure e
      "39056" -> do
        discardFromHand you (magnetic + 1)
        pure $ MojoGenreMinion (attrs `With` (GenreMinionMeta defeater (magnetic + 1)))
      _ -> pure e
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      case toCardCode attrs of
        "39054" -> exhaustChosen you True
        "39058" -> do
          upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
          unless (null upgrades) $ chooseOne you [targeting uid do
            card <- getsGame (`cardInPlay` UpgradeRef uid)
            for_ card $ \case
              PlayerCard c -> do
                removeFromPlay uid
                cardSpecific attrs "holdDefeated" c
              _ -> pure ()
            | uid <- upgrades]
        _ -> pure ()
      pure e
    RanAbility _ (isTarget attrs -> True) 3 _ _ -> do
      if minionDamage attrs > 0 then healAllDamage (minionId attrs) else giveStatus (minionId attrs) ToughStatus
      pure e
    RanAbility you (isTarget attrs -> True) 4 _ _ -> e <$ dealIndirectDamage you attrs 2
    RanAbility you (isTarget attrs -> True) 5 _ _ -> e <$ discardFromDeck you 3 attrs
    WithDiscarded (isTarget attrs -> True) FromDeck cards | toCardCode attrs == "39068" -> do
      let resources = nub $ concatMap (map snd . filter ((== PrintedResource) . fst) . cdResources . getCardDef) cards
      dealIndirectDamage (minionEngagedIdentity attrs) attrs (fromIntegral $ length resources)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        "39054" -> exhaustChosen you False
        "39055" -> giveStatus you StunnedStatus
        "39057" -> dealIndirectDamage you attrs 2
        _ -> pure ()
      pure e
    _ -> MojoGenreMinion . (`With` (GenreMinionMeta defeater magnetic)) <$> runMessage msg attrs
   where
    friendly = \case IdentityCharacter _ -> True; AllyCharacter _ -> True; _ -> False
    exhaustChosen you damages = do
      identities <- selectList (if damages then IdentityWithId you else IdentityWithId you <> UnexhaustedIdentity)
      allies <- selectList (if damages then AllyControlledBy (IdentityWithId you) else AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
      let chars = map IdentityCharacter identities <> map AllyCharacter allies
      unless (null chars) $ chooseOne you [targeting (characterRef c) do
        case c of IdentityCharacter p -> exhaust p; AllyCharacter aid -> exhaust aid; _ -> pure ()
        when damages $ damageCharacter c attrs (toDamage 1 FromAbility)
        | c <- chars]
