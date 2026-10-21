import { computed, inject, provide, type ComputedRef, type InjectionKey } from 'vue'
import { useI18n } from 'vue-i18n'
import { handleI18n } from '../i18n'
import type { ChoiceView } from '../server'

/**
 * The open question's choices, for the board: each card finds the choices about it, so a
 * card with something to do lights up and a click answers (or opens a menu, for several).
 */
export interface Choices {
  /** The choices about each card or entity, by its id. */
  byTarget: ComputedRef<Map<string, ChoiceView[]>>
  choose: (index: number) => void
}

const key: InjectionKey<Choices> = Symbol('choices')

export const provideChoices = (choices: () => ChoiceView[], choose: (index: number) => void) => {
  const byTarget = computed(() => {
    const map = new Map<string, ChoiceView[]>()
    for (const choice of choices()) {
      if (!choice.target) continue
      map.set(choice.target, [...(map.get(choice.target) ?? []), choice])
    }
    return map
  })
  const provided = { byTarget, choose }
  provide(key, provided)
  return provided
}

export const useChoices = () => inject(key, { byTarget: computed(() => new Map()), choose: () => {} })

/**
 * What a choice says, in the player's language. A `$` label is a key; plain text is shown
 * as it is; a choice the engine left unlabelled is named by what it does, and an ability
 * by its type.
 */
export const useChoiceLabel = () => {
  const { t, te } = useI18n()
  const keyed = (body: string) => {
    if (!body.startsWith('$')) return body
    const name = body.slice(1).split(' ')[0]
    // a key the locale lacks yet reads better as its last word than as the key
    if (!te(name)) return (name.split('.').pop() ?? name).replace(/([a-z])([A-Z])/g, '$1 $2').replace(/^./, (c) => c.toUpperCase())
    return handleI18n(body, t)
  }
  // `cardName` names what it is about: on its own card a chip says only what it does,
  // elsewhere a button says what to
  return (choice: ChoiceView, cardName = '', named = false): string => {
    if (choice.label && choice.kind !== 'ability') return keyed(choice.label)
    if (choice.kind === 'ability') {
      const type = choice.abilityType ? (te(`game.abilityTypes.${choice.abilityType}`) ? t(`game.abilityTypes.${choice.abilityType}`) : choice.abilityType) : t('game.choices.ability')
      const what = choice.label ? `${type}: ${keyed(choice.label)}` : type
      return named && cardName ? `${cardName} — ${what}` : what
    }
    const kind = `game.${named && cardName ? 'choicesNamed' : 'choices'}.${choice.kind}`
    return te(kind) ? t(kind, { card: cardName }) : cardName || choice.kind
  }
}
