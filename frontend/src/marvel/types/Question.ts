import { JsonDecoder } from 'ts.data.json'
import { PlayerCard, playerCardDecoder } from '@/marvel/types/PlayerCard'

export type Choice = EndTurn
  | UseAbility
  | PlayCard
  | PayWithCard
  | FinishPayment
  | Label
  | Defend
  | AllyDefend
  | TargetLabel

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

export interface Defend {
  tag: 'Defend'
}

export interface AllyDefend {
  tag: 'AllyDefend'
  contents: string
}

export interface Recover {
  tag: 'Recover'
}

export interface Attack {
  tag: 'Attack'
}

export interface Heal {
  tag: 'Heal'
}

export interface CreateEffect {
  tag: 'CreateEffect'
}

export interface AllyAttack {
  tag: 'AllyAttack'
}

export interface AllyThwart {
  tag: 'AllyThwart'
}

export interface Thwart {
  tag: 'Thwart'
}

export interface RemoveThreat {
  tag: 'RemoveThreat'
}

export interface PayWithCard {
  tag: 'PayWithCard',
  contents: PlayerCard,
}

export const endTurnDecoder = JsonDecoder.object<EndTurn>({ tag: JsonDecoder.isExactly('EndTurn') }, 'EndTurn')

export const finishPaymentDecoder = JsonDecoder.object<FinishPayment>({ tag: JsonDecoder.isExactly('FinishPayment') }, 'FinishPayment')

export const recoverDecoder = JsonDecoder.object<Recover>({ tag: JsonDecoder.isExactly('Recover') }, 'Recover')

export const attackDecoder = JsonDecoder.object<Attack>({ tag: JsonDecoder.isExactly('Attack') }, 'Attack')
export const healDecoder = JsonDecoder.object<Heal>({ tag: JsonDecoder.isExactly('Heal') }, 'Heal')
export const createEffectDecoder = JsonDecoder.object<CreateEffect>({ tag: JsonDecoder.isExactly('CreateEffect') }, 'CreateEffect')
export const allyAttackDecoder = JsonDecoder.object<AllyAttack>({ tag: JsonDecoder.isExactly('AllyAttack') }, 'AllyAttack')
export const allyThwartDecoder = JsonDecoder.object<AllyThwart>({ tag: JsonDecoder.isExactly('AllyThwart') }, 'AllyThwart')

export const thwartDecoder = JsonDecoder.object<Thwart>({ tag: JsonDecoder.isExactly('Thwart') }, 'Thwart')

export const removeThreatDecoder = JsonDecoder.object<RemoveThreat>({ tag: JsonDecoder.isExactly('RemoveThreat') }, 'RemoveThreat')

export interface UseAbility {
  tag: 'UseAbility'
  contents: UseAbilityContents
}

export interface Source {
  tag: string,
  contents: string
}

interface EnemyVillainId { tag: 'EnemyVillainId', contents: string }
interface EnemyMinionId { tag: 'EnemyMinionId', contents: string }

interface VillainCharacter { tag: 'VillainCharacter', contents: string }
interface MinionCharacter { tag: 'MinionCharacter', contents: string }
interface IdentityCharacter { tag: 'IdentityCharacter', contents: string }
interface AllyCharacter { tag: 'AllyCharacter', contents: string }

export type TargetContents
  = EnemyVillainId
  | EnemyMinionId
  | VillainCharacter
  | MinionCharacter
  | IdentityCharacter
  | AllyCharacter
  | string

export interface Target {
  tag: string,
  contents: TargetContents
}

export const sourceDecoder = JsonDecoder.object<Source>({ tag: JsonDecoder.string, contents: JsonDecoder.string }, 'Source')

export const targetDecoder = JsonDecoder.object<Target>(
  {
    tag: JsonDecoder.string,
    contents: JsonDecoder.oneOf<TargetContents>([
      JsonDecoder.object<EnemyVillainId>({ tag: JsonDecoder.isExactly('EnemyVillainId'), contents: JsonDecoder.string }, 'EnemyVillainId'),
      JsonDecoder.object<EnemyMinionId>({ tag: JsonDecoder.isExactly('EnemyMinionId'), contents: JsonDecoder.string }, 'EnemyMinionId'),
      JsonDecoder.object<VillainCharacter>({ tag: JsonDecoder.isExactly('VillainCharacter'), contents: JsonDecoder.string }, 'VillainCharacter'),
      JsonDecoder.object<MinionCharacter>({ tag: JsonDecoder.isExactly('MinionCharacter'), contents: JsonDecoder.string }, 'MinionCharacter'),
      JsonDecoder.object<AllyCharacter>({ tag: JsonDecoder.isExactly('AllyCharacter'), contents: JsonDecoder.string }, 'AllyCharacter'),
      JsonDecoder.object<IdentityCharacter>({ tag: JsonDecoder.isExactly('IdentityCharacter'), contents: JsonDecoder.string }, 'IdentityCharacter'),
      JsonDecoder.object<EnemyMinionId>({ tag: JsonDecoder.isExactly('EnemyMinionId'), contents: JsonDecoder.string }, 'EnemyMinionId'),
      JsonDecoder.string
    ], 'TargetContents')
  }, 'Target')


export const changeFormDecoder = JsonDecoder.object<ChangeForm>({ tag: JsonDecoder.isExactly('ChangeForm') }, 'ChangeForm')

export const labelDecoder = JsonDecoder.object<Label>({
  tag: JsonDecoder.isExactly('Label'),
  contents: JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.succeed], '[string, any]').map(r => r[0])
}, 'Label')
export const targetLabelDecoder = JsonDecoder.object<TargetLabel>({
  tag: JsonDecoder.isExactly('TargetLabel'),
  target: JsonDecoder.tuple([targetDecoder, JsonDecoder.succeed], '[Target]').map(a => a[0])
}, 'Label', { target: 'contents' })

export const youDrawCardsDecoder = JsonDecoder.object<YouDrawCards>({
  tag: JsonDecoder.isExactly('YouDrawCards'),
}, 'YouDrawCards')

export const chooseDamageDecoder = JsonDecoder.object<ChooseDamage>({
  tag: JsonDecoder.isExactly('ChooseDamage'),
}, 'ChooseDamage')

export const returnTargetToHandDecoder = JsonDecoder.object<ReturnTargetToHand>({
  tag: JsonDecoder.isExactly('ReturnTargetToHand'),
}, 'ReturnTargetToHand')

export const defendDecoder = JsonDecoder.object<Defend>({ tag: JsonDecoder.isExactly('Defend') }, 'Defend')

export const allyDefendDecoder = JsonDecoder.object<AllyDefend>({
  tag: JsonDecoder.isExactly('AllyDefend'),
  contents: JsonDecoder.tuple([JsonDecoder.string, JsonDecoder.succeed], '[string, any]').map(r => r[0])
}, 'AllyDefend')


export const payDecoder = JsonDecoder.object<Pay>({ tag: JsonDecoder.isExactly('Pay') }, 'Pay')
export const runDecoder = JsonDecoder.object<Run>({ tag: JsonDecoder.isExactly('Run') }, 'Run')
export const runAbilityDecoder = JsonDecoder.object<RunAbility>({ tag: JsonDecoder.isExactly('RunAbility') }, 'RunAbility')


export const abilityChoiceDecoder = JsonDecoder.oneOf<AbilityChoice>([
  changeFormDecoder,
  payDecoder,
  runDecoder,
  runAbilityDecoder,
  recoverDecoder,
  attackDecoder,
  healDecoder,
  createEffectDecoder,
  allyAttackDecoder,
  allyThwartDecoder,
  removeThreatDecoder,
  targetLabelDecoder,
  youDrawCardsDecoder,
  chooseDamageDecoder,
  returnTargetToHandDecoder,
  thwartDecoder], 'AbilityChoice')

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
  JsonDecoder.isExactly('Basic'),
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
  contents: JsonDecoder.tuple([playerCardDecoder, JsonDecoder.succeed], '[playerCard, window]').map(r => r[0]),
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
  'Special' |
  'Basic'

export interface UseAbilityContents {
  abilityChoices: AbilityChoice[]
  abilitySource: Source
  abilityLabel: string | null
  abilityType: AbilityType
}

type AbilityChoice = ChangeForm | Pay | Run | RunAbility | Recover | AllyAttack | Attack | Heal | AllyThwart | Thwart | CreateEffect | RemoveThreat | TargetLabel | YouDrawCards | ChooseDamage | ReturnTargetToHand

export interface ChangeForm {
  tag: 'ChangeForm'
}

export interface YouDrawCards {
  tag: 'YouDrawCards'
}

export interface ChooseDamage {
  tag: 'ChooseDamage'
}

export interface ReturnTargetToHand {
  tag: 'ReturnTargetToHand'
}

export interface Label {
  tag: 'Label'
  contents: string
}

export interface TargetLabel {
  tag: 'TargetLabel'
  target: Target
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

export type Question = ChooseOne | ChooseOneAtATime | ChoosePayment

export interface ChooseOne {
  tag: 'ChooseOne'
  contents: Choice[]
}

export interface ChooseOneAtATime {
  tag: 'ChooseOneAtATime'
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
  , defendDecoder
  , allyDefendDecoder
  , targetLabelDecoder
  ], 'Question'
)

export const chooseOneDecoder = JsonDecoder.object<ChooseOne>(
  {
    tag: JsonDecoder.isExactly('ChooseOne'),
    contents: JsonDecoder.array<Choice>(choiceDecoder, 'Choice[]')
  }, 'ChooseOne')

export const chooseOneAtATimeDecoder = JsonDecoder.object<ChooseOneAtATime>(
  {
    tag: JsonDecoder.isExactly('ChooseOneAtATime'),
    contents: JsonDecoder.array<Choice>(choiceDecoder, 'Choice[]')
  }, 'ChooseOneAtATime')

export const choosePaymentDecoder = JsonDecoder.object<ChoosePayment>(
  {
    tag: JsonDecoder.isExactly('ChoosePayment'),
  }, 'ChoosePayment')

export const questionDecoder = JsonDecoder.oneOf<Question>(
  [ chooseOneDecoder
  , chooseOneAtATimeDecoder
  , choosePaymentDecoder
  ], 'Question'
)
