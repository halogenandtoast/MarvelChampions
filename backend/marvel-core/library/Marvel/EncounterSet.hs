module Marvel.EncounterSet where

import Marvel.Prelude

import Marvel.Card.Code

data EncounterSet
  = Rhino
  | BombScare
  | Standard
  | SpiderManNemesis
  | SheHulkNemesis
  | BlackPantherNemesis
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

getNemesisSet :: CardCode -> EncounterSet
getNemesisSet cardCode = case toBaseCardCode cardCode of
  "01001" -> SpiderManNemesis
  "01019" -> SheHulkNemesis
  "01040" -> BlackPantherNemesis
  cCode ->
    error
      $ "No nemesis set for "
      <> show cCode
      <> " are you sure you've added it to EncounterSet.hs"
