module Marvel.EncounterSet where

import Marvel.Prelude

import Marvel.Card.Code

data EncounterSet
  = Rhino
  | BombScare
  | Klaw
  | MastersOfEvil
  | Standard
  | SpiderManNemesis
  | CaptainMarvelNemesis
  | SheHulkNemesis
  | IronManNemesis
  | BlackPantherNemesis
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

getNemesisSet :: CardCode -> EncounterSet
getNemesisSet cardCode = case toBaseCardCode cardCode of
  "01001" -> SpiderManNemesis
  "01010" -> CaptainMarvelNemesis
  "01019" -> SheHulkNemesis
  "01029" -> IronManNemesis
  "01040" -> BlackPantherNemesis
  cCode ->
    error
      $ "No nemesis set for "
      <> show cCode
      <> " are you sure you've added it to EncounterSet.hs"
