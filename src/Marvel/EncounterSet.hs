module Marvel.EncounterSet where

import Marvel.Prelude

import Marvel.Card.Code

data EncounterSet
  = Rhino
  | BombScare
  | Klaw
  | MastersOfEvil
  | Ultron
  | UnderAttack
  | LegionsOfHydra
  | TheDoomsdayChair
  | Standard
  | ExpertSet
  | SpiderManNemesis
  | CaptainMarvelNemesis
  | SheHulkNemesis
  | IronManNemesis
  | BlackPantherNemesis
  | CaptainAmericaNemesis
  | MsMarvelNemesis
  | BlackWidowNemesis
  | ThorNemesis
  | DoctorStrangeNemesis
  | HulkNemesis
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | The modular encounter sets of the core set, any of which a scenario may be played
-- with in place of the one it recommends.
modularSets :: [EncounterSet]
modularSets = [BombScare, MastersOfEvil, UnderAttack, LegionsOfHydra, TheDoomsdayChair]

getNemesisSet :: CardCode -> EncounterSet
getNemesisSet cardCode = case toBaseCardCode cardCode of
  "01001" -> SpiderManNemesis
  "01010" -> CaptainMarvelNemesis
  "01019" -> SheHulkNemesis
  "01029" -> IronManNemesis
  "01040" -> BlackPantherNemesis
  "03001" -> CaptainAmericaNemesis
  "05001" -> MsMarvelNemesis
  "08001" -> BlackWidowNemesis
  "06001" -> ThorNemesis
  "09001" -> DoctorStrangeNemesis
  "10001" -> HulkNemesis
  cCode ->
    error $
      "No nemesis set for "
        <> show cCode
        <> " are you sure you've added it to EncounterSet.hs"
