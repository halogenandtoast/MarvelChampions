import { JsonDecoder } from 'ts.data.json'

export type Choice = EndTurn | UseAbility

export interface EndTurn {
  tag: 'EndTurn'
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
