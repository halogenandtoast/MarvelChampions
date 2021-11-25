module Marvel.Trait where

import Marvel.Prelude

data Trait
  = Aerial
  | Android
  | Armor
  | Attack
  | Attorney
  | Avenger
  | Brute
  | Condition
  | Criminal
  | Defender
  | Defense
  | Elite
  | Gamma
  | Genius
  | HeroForHire
  | Hydra
  | Item
  | Location
  | MastersOfEvil
  | Persona
  | Shield
  | Skill
  | Soldier
  | Spy
  | Superpower
  | Tactic
  | Tech
  | Thwart
  | Weapon
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
