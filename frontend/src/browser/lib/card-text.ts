/**
 * MarvelCDB card text, parsed into tokens so nothing reaches `v-html`.
 *
 * The text is light HTML (`<b>`, `<i>`, `<em>`, `<hr>`) with icons written `[energy]`,
 * traits written `[[Avenger]]`, and hard line breaks.
 */

export type TextToken =
  | { kind: 'text'; value: string }
  | { kind: 'break' }
  | { kind: 'rule' }
  | { kind: 'icon'; name: IconName }
  | { kind: 'trait'; value: string }
  | { kind: 'emphasis'; style: 'bold' | 'italic'; children: TextToken[] }

export const ICON_NAMES = [
  'energy', 'mental', 'physical', 'wild', 'boost', 'star', 'per_hero', 'crisis', 'acceleration', 'amplify', 'hazard', 'cost', 'unique',
] as const
export type IconName = (typeof ICON_NAMES)[number]
const ICONS = new Set<string>(ICON_NAMES)

const TAG = /<(\/?)(b|i|em|strong|hr)\s*\/?>/gi
const INLINE = /\[\[([^\]]+)\]\]|\[([a-z_ ]+)\]/gi

function tokenizeRun(run: string): TextToken[] {
  const tokens: TextToken[] = []
  run.split('\n').forEach((line, i) => {
    if (i > 0) tokens.push({ kind: 'break' })
    let cursor = 0
    for (const match of line.matchAll(INLINE)) {
      const at = match.index ?? 0
      const icon = match[2]?.toLowerCase().replace(' ', '_')
      if (match[2] && !ICONS.has(icon ?? '')) continue
      if (at > cursor) tokens.push({ kind: 'text', value: line.slice(cursor, at) })
      tokens.push(match[1] ? { kind: 'trait', value: match[1] } : { kind: 'icon', name: icon as IconName })
      cursor = at + match[0].length
    }
    if (cursor < line.length) tokens.push({ kind: 'text', value: line.slice(cursor) })
  })
  return tokens
}

/** Parse card text into a token tree, tolerating unclosed and mismatched tags. */
export function parseCardText(input: string | null | undefined): TextToken[] {
  if (!input) return []
  const root: TextToken[] = []
  const stack: TextToken[][] = [root]
  const open: string[] = []
  const push = (tokens: TextToken[]) => stack[stack.length - 1]?.push(...tokens)
  let cursor = 0
  for (const match of input.matchAll(TAG)) {
    const at = match.index ?? 0
    if (at > cursor) push(tokenizeRun(input.slice(cursor, at)))
    cursor = at + match[0].length
    const tag = match[2].toLowerCase()
    if (tag === 'hr') {
      push([{ kind: 'rule' }])
      continue
    }
    const style = tag === 'b' || tag === 'strong' ? 'bold' : 'italic'
    if (match[1] === '/') {
      const depth = open.lastIndexOf(style)
      if (depth === -1) continue
      while (open.length > depth) {
        open.pop()
        stack.pop()
      }
    } else {
      const node: TextToken = { kind: 'emphasis', style, children: [] }
      push([node])
      open.push(style)
      stack.push(node.children)
    }
  }
  if (cursor < input.length) push(tokenizeRun(input.slice(cursor)))
  return root
}
