module Marvel.Obligation.Obligations.MojoGenres where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Id ()
import Marvel.Identity.Types (Field (PlayerIdentityHand, PlayerIdentityDiscard))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

newtype GenreObligationMeta = GenreObligationMeta {clueCounters :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MojoGenreObligation = MojoGenreObligation (Attrs Obligation `With` GenreObligationMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsObligation MojoGenreObligation where
  toObligationAttrs (MojoGenreObligation (a `With` _)) = a
  obligationDisplayCounters (MojoGenreObligation (_ `With` meta)) = clueCounters meta

mkGenreObligation :: Attrs Obligation -> MojoGenreObligation
mkGenreObligation a = MojoGenreObligation (a `With` (GenreObligationMeta 0))

mojo39036, mojo39061, mojo39062, mojo39063, mojo39064, mojo39065 :: ObligationCard MojoGenreObligation
mojo39036 = obligation mkGenreObligation Cards.mojo39036
mojo39061 = obligation mkGenreObligation Cards.mojo39061
mojo39062 = obligation mkGenreObligation Cards.mojo39062
mojo39063 = obligation mkGenreObligation Cards.mojo39063
mojo39064 = obligation mkGenreObligation Cards.mojo39064
mojo39065 = obligation mkGenreObligation Cards.mojo39065

instance HasModifiersFor MojoGenreObligation where
  getBlankingModifiersFor _ target (MojoGenreObligation (a `With` _))
    | toCardCode a == "39061", Just you <- obligationOwner a, SupportRef sid <- target = do
        controlled <- elem sid <$> selectList (SupportControlledBy $ IdentityWithId you)
        pure [Blank | controlled]
  getBlankingModifiersFor _ _ _ = pure []
  getModifiersFor source target e@(MojoGenreObligation (a `With` _)) = do
    blanking <- getBlankingModifiersFor source target e
    def <- getsGame (`abilityCardDef` target)
    let peril = [KeywordModifier Peril | toCardCode a == "39064", isJust (obligationOwner a), maybe False (isJust . cdEncounterSet) def]
    pure $ blanking <> peril <> case target of
      IdentityRef you | Just you == obligationOwner a -> case toCardCode a of
        "39062" -> [AdditionalResourceCost (CardWithType UpgradeType) 2]
        "39063" -> [AllyLimitReduction 2]
        _ -> []
      _ -> []

instance HasAbilities MojoGenreObligation where
  getAbilities (MojoGenreObligation (a `With` _)) =
    [windowAbility a 1 (W.SideSchemeDefeated W.After AnySideScheme) ForcedResponse NoCost (runAbility a 1) | code == "39036", isJust owner]
    <> [ability a 2 AlterEgoAction (own <> NotCriteria (SupportExists $ SupportMatches [SupportControlledBy You, SupportWithExhaustion True])) ExhaustCost (runAbility a 2) | code == "39061", isJust owner]
    <> [ability a 3 AlterEgoAction (own <> AnyCriteria [UpgradeExists $ UpgradeControlledBy You <> UpgradeThatCanBeDiscarded, ExtendedCardExists $ InHandOf You $ BasicCardMatches $ CardWithType UpgradeType]) NoCost (runAbility a 3) | code == "39062", isJust owner]
    <> [ability a 4 AlterEgoAction (own <> ExtendedCardExists (InDiscardOf You $ BasicCardMatches $ CardWithType AllyType)) (ExhaustCharacterCost (CharacterControlledBy You) <> ExhaustCharacterCost (CharacterControlledBy You)) (runAbility a 4) | code == "39063", isJust owner]
    <> [ability a 5 AlterEgoAction own (DiscardHandCardsCost 3 $ Just 3) (runAbility a 5) | code == "39064", isJust owner]
    <> [ability a 6 AlterEgoAction (own <> SelfMatches ConfusedIdentity) ExhaustCost (runAbility a 6) | code == "39065", isJust owner]
    -- Looking up a rule is an out-of-game act. This explicit control records its
    -- mandatory consequence without guessing whether the player read a rulebook.
    <> [(ability a 7 Action own NoCost (runAbility a 7)) {abilityLabel = Just "$label.mojoLookUpRule"} | code == "39065", isJust owner]
   where
    code = toCardCode a
    owner = obligationOwner a
    own = maybe Never (SelfMatches . IdentityWithId) owner

instance RunMessage MojoGenreObligation where
  runMessage msg e@(MojoGenreObligation (attrs `With` (GenreObligationMeta clues))) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      when (toCardCode attrs == "39063") $ identityStep you CheckAllyLimit
      MojoGenreObligation . (`With` (GenreObligationMeta clues)) <$> runMessage msg attrs
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      when (clues + 1 >= 3) $ removeFromPlay attrs
      pure $ MojoGenreObligation (attrs `With` (GenreObligationMeta (clues + 1)))
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      selectList (SupportControlledBy $ IdentityWithId you) >>= traverse_ exhaust
      removeFromPlay attrs
      pure e
    RanAbility you (isTarget attrs -> True) 3 _ _ -> do
      upgrades <- selectList (UpgradeControlledBy (IdentityWithId you) <> UpgradeThatCanBeDiscarded)
      hand <- filter (cardMatch $ CardWithType UpgradeType) . unHand <$> project PlayerIdentityHand you
      let choices = [targeting uid (discardUpgrade uid) | uid <- upgrades] <> [targeting c (discardCard c) | c <- hand]
      unless (null choices) $ do
        chooseOne you choices
        cardSpecific attrs "checkUpgrades" you
      pure e
    CardSpecific (isTarget attrs -> True) "checkUpgrades" (fromJSON -> Success you) -> do
      controlled <- length <$> selectList (UpgradeControlledBy $ IdentityWithId you)
      discarded <- length . filter (cardMatch $ CardWithType UpgradeType) . unDiscard <$> project PlayerIdentityDiscard you
      when (discarded > controlled) $ removeFromPlay attrs
      pure e
    RanAbility you (isTarget attrs -> True) 4 _ _ -> do
      cards <- filter (cardMatch $ CardWithType AllyType) . unDiscard <$> project PlayerIdentityDiscard you
      unless (null cards) $ chooseOne you [targeting c (shuffleIntoDeck you [c] >> removeFromPlay attrs) | c <- cards]
      pure e
    RanAbility you (isTarget attrs -> True) 5 _ _ -> do
      players <- getPlayers
      unless (null players) $ chooseOne you [targeting player (drawCards player 1 >> removeFromPlay attrs) | player <- players]
      pure e
    RanAbility you (isTarget attrs -> True) 6 _ _ -> do
      removeStatus you ConfusedStatus
      removeFromPlay attrs
      pure e
    RanAbility you (isTarget attrs -> True) 7 _ _ -> e <$ giveStatus you ConfusedStatus
    _ -> MojoGenreObligation . (`With` (GenreObligationMeta clues)) <$> runMessage msg attrs
