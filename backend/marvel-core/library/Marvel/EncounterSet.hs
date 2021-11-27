module Marvel.EncounterSet where

import Marvel.Prelude

import Marvel.Card.Code

data EncounterSet
  = Rhino
  | BombScare
  | Standard
  | SpiderManNemesis
  | SheHulkNemesis
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

getNemesisSet :: CardCode -> EncounterSet
getNemesisSet "01001" = SpiderManNemesis
getNemesisSet "01019" = SheHulkNemesis
getNemesisSet cardCode =
  error
    $ "No nemesis set for "
    <> show cardCode
    <> " are you sure you've added it to EncounterSet.hs"
