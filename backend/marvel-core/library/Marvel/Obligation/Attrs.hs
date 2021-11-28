{-# LANGUAGE TemplateHaskell #-}
module Marvel.Obligation.Attrs where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target

class IsObligation a

type ObligationCard a = CardBuilder ObligationId a

data ObligationAttrs = ObligationAttrs
  { obligationId :: ObligationId
  , obligationCardDef :: CardDef
  , obligationSurge :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''ObligationAttrs

instance HasCardCode ObligationAttrs where
  toCardCode = toCardCode . obligationCardDef

obligation :: (ObligationAttrs -> a) -> CardDef -> CardBuilder ObligationId a
obligation f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid -> f $ ObligationAttrs
    { obligationId = mid
    , obligationCardDef = cardDef
    , obligationSurge = False
    }
  }

instance Entity ObligationAttrs where
  type EntityId ObligationAttrs = ObligationId
  type EntityAttrs ObligationAttrs = ObligationAttrs
  toId = obligationId
  toAttrs = id

instance IsSource ObligationAttrs where
  toSource = ObligationSource . toId

instance IsTarget ObligationAttrs where
  toTarget = ObligationTarget . toId

instance IsCard ObligationAttrs where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unObligationId $ toId a
    , ecCardDef = getCardDef a
    }

instance HasCardDef ObligationAttrs where
  getCardDef = obligationCardDef

instance RunMessage ObligationAttrs where
  runMessage msg attrs = case msg of
    ObligationMessage tid msg' | tid == toId attrs -> case msg' of
      ResolvedObligation identityId -> do
        pushAll
          $ RemoveFromPlay (toTarget attrs)
          : [ Surge identityId | obligationSurge attrs ]
        pure attrs
      _ -> pure attrs
    GainSurge target | isTarget attrs target -> pure $ attrs & surgeL .~ True
    _ -> pure attrs
