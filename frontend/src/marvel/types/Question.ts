import { JsonDecoder } from 'ts.data.json'
import { PlayerCard, playerCardDecoder } from '@/marvel/types/Identity'

export type Choice = EndTurn | UseAbility | PlayCard | PayWithCard | FinishPayment

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

export const payDecoder = JsonDecoder.object<Pay>({ tag: JsonDecoder.isExactly('Pay') }, 'Pay')


export const abilityChoiceDecoder = JsonDecoder.oneOf<AbilityChoice>([changeFormDecoder, payDecoder], 'AbilityChoice')

export const useAbilityContentsDecoder = JsonDecoder.object<UseAbilityContents>({
  abilityChoice: abilityChoiceDecoder,
  abilityLabel: JsonDecoder.nullable(JsonDecoder.string),
  abilitySource: sourceDecoder,
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

export interface UseAbilityContents {
  abilityChoice: AbilityChoice
  abilitySource: Source
  abilityLabel: string | null
}

type AbilityChoice = ChangeForm | Pay

export interface ChangeForm {
  tag: 'ChangeForm'
}

export interface Pay {
  tag: 'Pay'
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
