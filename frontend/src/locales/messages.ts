import { uiLocaleFor, type UiLocale } from './language'

const localeLoaders = {
  en: () => import('./en'),
} satisfies Record<UiLocale, () => Promise<unknown>>

export type SupportedLocale = keyof typeof localeLoaders

export const supportedLocales = Object.keys(localeLoaders) as SupportedLocale[]

export function normalizeLocale(locale: string): SupportedLocale {
  return uiLocaleFor(locale)
}

export async function loadLocaleMessages(locale: string) {
  const normalizedLocale = normalizeLocale(locale)
  const messages = await localeLoaders[normalizedLocale]()
  return { locale: normalizedLocale, messages: messages.default }
}
