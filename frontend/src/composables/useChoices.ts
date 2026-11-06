import { computed, inject, provide, ref, type Ref, type ComputedRef, type InjectionKey } from 'vue'
import { useI18n } from 'vue-i18n'
import { handleI18n } from '../i18n'
import type { ChoiceView } from '../server'

/**
 * The open question's choices, for the board: each card finds the choices about it, so a
 * card with something to do lights up and a click answers (or opens a menu, for several).
 */
export interface Choices {
  hoveredTarget: Ref<string | null>
  /** Damage allocated while answering the question, by character id. */
  assignedDamage: ComputedRef<Record<string, number>>
  /** The choices about each card or entity, by its id. */
  byTarget: ComputedRef<Map<string, ChoiceView[]>>
  choose: (index: number) => void
  /** the card the open question is about */
  sourceId: ComputedRef<string | null>
}

const key: InjectionKey<Choices> = Symbol('choices')

export const provideChoices = (choices: () => ChoiceView[], choose: (index: number) => void, source: () => string | null = () => null, assignedDamage: () => Record<string, number> = () => ({})) => {
  const byTarget = computed(() => {
    const map = new Map<string, ChoiceView[]>()
    for (const choice of choices()) {
      if (!choice.target) continue
      map.set(choice.target, [...(map.get(choice.target) ?? []), choice])
    }
    return map
  })
  const provided = { hoveredTarget: ref<string | null>(null), assignedDamage: computed(assignedDamage), byTarget, choose, sourceId: computed(source) }
  provide(key, provided)
  return provided
}

export const useChoices = () => inject(key, { hoveredTarget: ref<string | null>(null), assignedDamage: computed<Record<string, number>>(() => ({})), byTarget: computed(() => new Map()), choose: () => {}, sourceId: computed(() => null) })

/** Server text that may be a key the locale lacks yet: then its last word, spaced out. */
export const useKeyed = () => {
  const { t, te } = useI18n()
  return (body: string) => {
    if (!body.startsWith('$')) return body
    const name = body.slice(1).split(' ')[0]
    // a key the locale lacks yet reads better as its last word than as the key
    if (!te(name)) return (name.split('.').pop() ?? name).replace(/([a-z])([A-Z])/g, '$1 $2').replace(/^./, (c) => c.toUpperCase())
    return handleI18n(body, t)
  }
}

/**
 * What a choice says, in the player's language. A `$` label is a key; plain text is shown
 * as it is; a choice the engine left unlabelled is named by what it does, and an ability
 * by its type.
 */
export const useChoiceLabel = () => {
  const { t, te } = useI18n()
  const keyed = useKeyed()
  // `cardName` names what it is about: on its own card a chip says only what it does,
  // elsewhere a button says what to
  return (choice: ChoiceView, cardName = '', named = false): string => {
    if (choice.label && choice.kind !== 'ability') return keyed(choice.label)
    if (choice.kind === 'ability') {
      const type = choice.abilityType ? (te(`game.abilityTypes.${choice.abilityType}`) ? t(`game.abilityTypes.${choice.abilityType}`) : choice.abilityType) : t('game.choices.ability')
      // a basic power is named by what it does: attack, thwart, recover
      const basic = choice.abilityType === 'Basic' || /^\$label\.(changeForm|changeTo[AB]|attack|thwart|recover|defend)\b/.test(choice.label ?? '')
      const what = choice.label ? (basic ? keyed(choice.label) : `${type}: ${keyed(choice.label)}`) : type
      return named && cardName ? `${cardName} — ${what}` : what
    }
    const kind = `game.${named && cardName ? 'choicesNamed' : 'choices'}.${choice.kind}`
    return te(kind) ? t(kind, { card: cardName }) : cardName || choice.kind
  }
}
