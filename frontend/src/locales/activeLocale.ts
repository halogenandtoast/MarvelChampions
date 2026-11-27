import { ref, type Ref } from 'vue'
import { normalizeLocale, type SupportedLocale } from './messages'

/** Shared outside Vue components so stores can react to language changes too. */
export const activeLocale: Ref<SupportedLocale> = ref('en')

export function setActiveLocale(locale: string): void {
  const normalized = normalizeLocale(locale)
  activeLocale.value = normalized
  document.documentElement.lang = normalized
}
