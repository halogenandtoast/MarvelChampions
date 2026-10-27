module Marvel.Ally.Allies.Cosmo (
  cosmo,
  Cosmo (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

cosmo :: AllyCard Cosmo
cosmo = ally (Cosmo . (`With` Meta Nothing False)) Cards.cosmo (Thw 1, 1) (Atk 1, 1) (HP 2)

-- | The card type named for this use, and whether the discarded card was of it.
data Meta = Meta {named :: Maybe CardType, safe :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Cosmo = Cosmo (Attrs Ally `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAlly Cosmo where
  toAllyAttrs (Cosmo (a `With` _)) = a

instance IsRef Cosmo where
  toRef = toRef . toAllyAttrs

-- | Interrupt: When Cosmo attacks or thwarts, name a card type, then discard the top
-- card of a player deck or the encounter deck. If that card is of the named type, Cosmo
-- does not take consequential damage for this use (errata).
instance HasAbilities Cosmo where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (W.WindowOneOf [W.AllyAttacked W.When self AnyEnemy, W.AllyThwarted W.When self AnyScheme])
        Interrupt
        OwnsThis
        NoCost
        (runAbility a 1)
    ]
   where
    self = AllyWithId (allyId $ toAllyAttrs a)

-- | No consequential damage for the use the discard answered: read as the attack lands
-- or the thwart's consequential damage is worked out, then gone.
instance HasModifiersFor Cosmo where
  getModifiersFor _ (AllyRef aid) (Cosmo (a `With` meta))
    | aid == allyId a && safe meta = pure [NoConsequentialDamage]
  getModifiersFor _ _ _ = pure []

instance RunMessage Cosmo where
  runMessage msg c@(Cosmo (attrs `With` meta)) = runQueueT $ case msg of
    RanAbility you (isTarget c -> True) 1 _ _ -> do
      chooseOne you [labeled (typeLabel t) (cardSpecific c "named" t) | t <- nameable]
      pure c
    CardSpecific (isTarget c -> True) "named" (fromJSON -> Success t) -> do
      players <- selectList AnyIdentity
      chooseOne (allyController attrs) $
        [targeting p (discardFromDeck p 1 c) | p <- players]
          <> [labeled "$label.encounterDeck" (discardTopOfEncounterDeckFor 1 c)]
      pure . Cosmo $ attrs `With` meta {named = Just t}
    WithDiscarded (isTarget c -> True) _ cards -> do
      let hit = any (\card -> Just (cdCardType $ getCardDef card) == named meta) cards
      pure . Cosmo $ attrs `With` Meta Nothing hit
    AllyMessage aid msg' | aid == allyId attrs, done msg' -> do
      -- the modifier is read from the game as this runs, then cleared
      attrs' <- runMessage msg attrs
      pure . Cosmo $ attrs' `With` Meta Nothing False
    _ -> Cosmo . (`With` meta) <$> runMessage msg attrs
   where
    done = \case
      AllyAttackLands {} -> True
      AllyThwartConsequential -> True
      _ -> False

-- | The card types a deck's top card can be.
nameable :: [CardType]
nameable =
  [ AllyType
  , EventType
  , ResourceType
  , SupportType
  , UpgradeType
  , AttachmentType
  , EnvironmentType
  , MinionType
  , ObligationType
  , SideSchemeType
  , TreacheryType
  ]

typeLabel :: CardType -> Text
typeLabel t = "$label.name" <> tshow t
