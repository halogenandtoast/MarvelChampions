// The languages the interface has messages for. Add one with its folder of messages in
// ./<code>, its ./<code>.ts and a loader in ./messages.ts.
export const uiLocales = ['en'] as const

export type UiLocale = (typeof uiLocales)[number]

export function uiLocaleFor(language: string): UiLocale {
  const normalized = language.toLowerCase()
  return uiLocales.includes(normalized as UiLocale) ? (normalized as UiLocale) : 'en'
}

export function preferredLanguage(browserLocale: string): string {
  const normalized = browserLocale.trim().toLowerCase()
  if (!normalized) return 'en'

  const [language, ...subtags] = normalized.split('-')
  if (language !== 'zh') return language

  if (subtags.includes('hant')) return 'zh'

  if (subtags.includes('hans') || subtags.includes('cn') || subtags.includes('sg')) {
    return 'zh-cn'
  }

  return 'zh'
}
