import { JsonDecoder } from 'ts.data.json'

export type Choice = EndTurn | UseAbility | PlayCard

export interface EndTurn {
  tag: 'EndTurn'
}

export interface PlayCard {
  tag: 'PlayCard'
}

export const endTurnDecoder = JsonDecoder.object<EndTurn>({ tag: JsonDecoder.isExactly('EndTurn') }, 'EndTurn')

export interface UseAbility {
  tag: 'UseAbility'
  contents: UseAbilityContents
}

export const changeFormDecoder = JsonDecoder.object<ChangeForm>({ tag: JsonDecoder.isExactly('ChangeForm') }, 'ChangeForm')

export const abilityChoiceDecoder = JsonDecoder.oneOf<AbilityChoice>([changeFormDecoder], 'AbilityChoice')

export const useAbilityContentsDecoder = JsonDecoder.object<UseAbilityContents>({
  abilityChoice: abilityChoiceDecoder
}, 'UseAbilityContents')

export const useAbilityDecoder = JsonDecoder.object<UseAbility>({
  tag: JsonDecoder.isExactly('UseAbility'),
  contents: useAbilityContentsDecoder
}, 'UseAbility')

export const playCardDecoder = JsonDecoder.object<PlayCard>({
  tag: JsonDecoder.isExactly('PlayCard'),
}, 'PlayCard')

export interface UseAbilityContents {
  abilityChoice: AbilityChoice
}

type AbilityChoice = ChangeForm

export interface ChangeForm {
  tag: 'ChangeForm'
}

export type Question = ChooseOne

export interface ChooseOne {
  tag: 'ChooseOne'
  contents: Choice[]
}

export const choiceDecoder = JsonDecoder.oneOf<Choice>(
  [ endTurnDecoder
  , useAbilityDecoder
  , playCardDecoder
  ], 'Question'
)

export const chooseOneDecoder = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.isExactly('ChooseOne'),
    contents: JsonDecoder.array<Choice>(choiceDecoder, 'Choice[]')
  }, 'ChooseOne')

export const questionDecoder = JsonDecoder.oneOf<Question>(
  [ chooseOneDecoder
  ], 'Question'
)
