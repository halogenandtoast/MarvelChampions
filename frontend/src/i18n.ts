import { useI18n } from 'vue-i18n'

// Text from the game server arrives as translation keys, as in Arkham: `$`, a dotted key
// into the locale files, and the key's variables inline, e.g.
//
//   $log.takesDamage name=s:"Gimli" __name=s:"01004" count=i:2
//
// `__name` carries the code of the card `name` names, for when card names are translated.

type Translate = (key: string, params: Record<string, unknown>) => string

const param = /([A-Za-z_][A-Za-z0-9_]*)=(?:i:(-?\d+)|s:"((?:[^"\\]|\\.)*)")/g

export const parseInput = (body: string): { key: string; params: Record<string, string | number> } => {
  const [head, ...rest] = body.slice(1).split(' ')
  const params: Record<string, string | number> = {}
  for (const [, name, int, text] of rest.join(' ').matchAll(param))
    params[name] = int !== undefined ? Number(int) : text.replace(/\\(.)/g, '$1')
  return { key: head, params }
}

/** Server text in the player's language; text that is not a key is shown as it is. */
export const handleI18n = (body: string, t: Translate): string => {
  if (!body.startsWith('$')) return body
  const { key, params } = parseInput(body)
  return t(key, params)
}

/** Build a key the way the server does, for text the client makes itself. */
export const ikey = (key: string, params: Record<string, string | number> = {}): string =>
  [
    `$${key}`,
    ...Object.entries(params).map(([name, value]) =>
      typeof value === 'number' ? `${name}=i:${value}` : `${name}=s:"${value.replace(/["\\]/g, '\\$&')}"`,
    ),
  ].join(' ')

/** Whether server text is the given key, whatever its variables. */
export const isKey = (body: string | undefined, key: string): boolean =>
  body === `$${key}` || (body?.startsWith(`$${key} `) ?? false)

/** Server text in the player's language, for templates: `const tr = useServerText()`. */
export const useServerText = () => {
  const { t } = useI18n()
  return (body: string | null | undefined): string => (body ? handleI18n(body, t) : '')
}
