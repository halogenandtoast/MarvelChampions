import { JsonDecoder } from 'ts.data.json'
import { PlayerCard, playerCardDecoder } from '@/marvel/types/Identity'

export type Choice = EndTurn | UseAbility | PlayCard | PayWithCard | FinishPayment | Label

export interface EndTurn {
  tag: 'EndTurn'
}

export interface FinishPayment {
  tag: 'FinishPayment'
}

export interface PlayCard {
  tag: 'PlayCard',
  contents: PlayerCard,
}

export interface PayWithCard {
  tag: 'PayWithCard',
  contents: PlayerCard,
}

export const endTurnDecoder = JsonDecoder.object<EndTurn>({ tag: JsonDecoder.isExactly('EndTurn') }, 'EndTurn')

export const finishPaymentDecoder = JsonDecoder.object<FinishPayment>({ tag: JsonDecoder.isExactly('FinishPayment') }, 'FinishPayment')

export interface UseAbility {
  tag: 'UseAbility'
  contents: UseAbilityContents
}

export interface Source {
  tag: string,
  contents: string
}

export const sourceDecoder = JsonDecoder.object<Source>({ tag: JsonDecoder.string, contents: JsonDecoder.string }, 'Source')


export const changeFormDecoder = JsonDecoder.object<ChangeForm>({ tag: JsonDecoder.isExactly('ChangeForm') }, 'ChangeForm')

export const labelDecoder = JsonDecoder.object<Label>({ tag: JsonDecoder.isExactly('Label') }, 'Label')


export const payDecoder = JsonDecoder.object<Pay>({ tag: JsonDecoder.isExactly('Pay') }, 'Pay')
export const runDecoder = JsonDecoder.object<Run>({ tag: JsonDecoder.isExactly('Run') }, 'Run')
export const runAbilityDecoder = JsonDecoder.object<RunAbility>({ tag: JsonDecoder.isExactly('RunAbility') }, 'RunAbility')


export const abilityChoiceDecoder = JsonDecoder.oneOf<AbilityChoice>([changeFormDecoder, payDecoder, runDecoder, runAbilityDecoder], 'AbilityChoice')

export const abilityTypeDecoder = JsonDecoder.oneOf<AbilityType>([
  JsonDecoder.isExactly('Interrupt'),
  JsonDecoder.isExactly('HeroInterrupt'),
  JsonDecoder.isExactly('ForcedInterrupt'),
  JsonDecoder.isExactly('Resource'),
  JsonDecoder.isExactly('HeroResource'),
  JsonDecoder.isExactly('Response'),
  JsonDecoder.isExactly('ForcedResponse'),
  JsonDecoder.isExactly('Action'),
  JsonDecoder.isExactly('HeroAction'),
  JsonDecoder.isExactly('AlterEgoAction'),
  JsonDecoder.isExactly('Special')], 'AbilityType')

export const useAbilityContentsDecoder = JsonDecoder.object<UseAbilityContents>({
  abilityChoices: JsonDecoder.array<AbilityChoice>(abilityChoiceDecoder, 'AbilityChoice[]'),
  abilityLabel: JsonDecoder.nullable(JsonDecoder.string),
  abilitySource: sourceDecoder,
  abilityType: abilityTypeDecoder,
}, 'UseAbilityContents')

export const useAbilityDecoder = JsonDecoder.object<UseAbility>({
  tag: JsonDecoder.isExactly('UseAbility'),
  contents: useAbilityContentsDecoder
}, 'UseAbility')

export const playCardDecoder = JsonDecoder.object<PlayCard>({
  tag: JsonDecoder.isExactly('PlayCard'),
  contents: playerCardDecoder,
}, 'PlayCard')

export const payWithCardDecoder = JsonDecoder.object<PayWithCard>({
  tag: JsonDecoder.isExactly('PayWithCard'),
  contents: playerCardDecoder,
}, 'PayWithCard')

export type AbilityType =
  'Interrupt' |
  'HeroInterrupt' |
  'ForcedInterrupt' |
  'Resource' |
  'HeroResource' |
  'Response' |
  'ForcedResponse' |
  'Action' |
  'HeroAction' |
  'AlterEgoAction' |
  'Special'

export interface UseAbilityContents {
  abilityChoices: AbilityChoice[]
  abilitySource: Source
  abilityLabel: string | null
  abilityType: AbilityType
}

type AbilityChoice = ChangeForm | Pay | Run | RunAbility

export interface ChangeForm {
  tag: 'ChangeForm'
}

export interface Label {
  tag: 'Label'
}

export interface Pay {
  tag: 'Pay'
}

export interface Run {
  tag: 'Run'
}

export interface RunAbility {
  tag: 'RunAbility'
}

export type Question = ChooseOne | ChoosePayment

export interface ChooseOne {
  tag: 'ChooseOne'
  contents: Choice[]
}

export interface ChoosePayment {
  tag: 'ChoosePayment'
}

export const choiceDecoder = JsonDecoder.oneOf<Choice>(
  [ endTurnDecoder
  , useAbilityDecoder
  , playCardDecoder
  , payWithCardDecoder
  , finishPaymentDecoder
  , labelDecoder
  ], 'Question'
)

export const chooseOneDecoder = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.isExactly('ChooseOne'),
    contents: JsonDecoder.array<Choice>(choiceDecoder, 'Choice[]')
  }, 'ChooseOne')

export const choosePaymentDecoder = JsonDecoder.object<ChoosePayment>(
  {
    tag: JsonDecoder.isExactly('ChoosePayment'),
  }, 'ChoosePayment')

export const questionDecoder = JsonDecoder.oneOf<Question>(
  [ chooseOneDecoder
  , choosePaymentDecoder
  ], 'Question'
)
