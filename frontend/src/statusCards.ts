export interface StatusCardCounts {
  stunned?: boolean
  confused?: boolean
  tough?: boolean
  stunCards?: number
  confuseCards?: number
  toughCards?: number
}

// A card can be present without its condition applying (Steady).
export const statusCards = (character: StatusCardCounts) => [
  ...Array<string>(character.stunCards ?? (character.stunned ? 1 : 0)).fill('stunned'),
  ...Array<string>(character.confuseCards ?? (character.confused ? 1 : 0)).fill('confused'),
  ...Array<string>(character.toughCards ?? (character.tough ? 1 : 0)).fill('tough'),
]

export const inactiveStatusCards = (character: StatusCardCounts) => [
  ...(character.stunned === false && (character.stunCards ?? 0) > 0 ? ['stunned'] : []),
  ...(character.confused === false && (character.confuseCards ?? 0) > 0 ? ['confused'] : []),
]
