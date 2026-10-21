// The structured game log, as Marvel.Log.Entry sends it. Mirrors Arkham's
// types/GameLog.ts: parts arrive already parsed, so rendering is a v-for.

export type LogRefKind =
  | 'RefCard'
  | 'RefPlayer'
  | 'RefIdentity'
  | 'RefAlly'
  | 'RefSupport'
  | 'RefUpgrade'
  | 'RefEvent'
  | 'RefVillain'
  | 'RefMinion'
  | 'RefTreachery'
  | 'RefAttachment'
  | 'RefObligation'
  | 'RefMainScheme'
  | 'RefSideScheme'
  | 'RefScenario'

/** Something in the game, drawn as a chip that previews its card. */
export interface LogRef {
  kind: LogRefKind
  name: string
  cardCode: string | null
  cardId: string | null
  entityId: string | null
  faceDown: boolean
}

export type LogPart =
  | { tag: 'LogText'; contents: string }
  /** An i18n key under log., with variables that are parts themselves. */
  | { tag: 'LogI18n'; contents: [string, Record<string, LogPart>] }
  | { tag: 'LogRefPart'; contents: LogRef }
  | { tag: 'LogNumber'; contents: number }
  /** drawn +2 / -1 */
  | { tag: 'LogDelta'; contents: number }
  /** a stat or resource icon by name: thwart, threat, energy */
  | { tag: 'LogIcon'; contents: string }
  /** joined by the locale's list rule */
  | { tag: 'LogList'; contents: LogPart[] }

export type LogKind = 'Structure' | 'Action' | 'Mechanic' | 'Test' | 'Notice' | 'Chat'

export type LogAudience =
  | { tag: 'Everyone' }
  | { tag: 'OnlyPlayer'; contents: string }
  | { tag: 'ExceptPlayer'; contents: string }

export type LogTone = 'Good' | 'Bad'

export type LogGroupRole = 'GroupHeader' | 'GroupMember' | 'GroupSummary'

export interface LogGroup {
  id: string
  role: LogGroupRole
}

export interface LogEntry {
  /** Monotonic per game: the entry's identity, and the cursor for scrolling back. */
  seq: number
  /** The step to undo to, to put the game back to just before this. */
  step: number | null
  kind: LogKind
  body: LogPart[]
  source: LogRef | null
  children: LogEntry[]
  audience: LogAudience
  context: { round: number; phase: string } | null
  tone: LogTone | null
  /** The block this entry belongs to. Rows stay flat; blocks are drawn here. */
  group: LogGroup | null
}

/** A run of entries sharing a group id, drawn as one block: a header that names it,
 * members that accumulate, and a summary it collapses to. Any may be missing, as an
 * undo deletes by step. */
export type LogGroupItem = {
  kind: 'group'
  id: string
  header: LogEntry | null
  members: LogItem[]
  summary: LogEntry | null
}

export type LogItem = { kind: 'entry'; entry: LogEntry } | LogGroupItem

/** Entries into items, collecting each group by id wherever its rows fall, as Arkham's
 * groupLogEntries. An id is a path: "a/b" is block b inside block a. */
export function groupLogEntries(entries: readonly LogEntry[]): LogItem[] {
  const items: LogItem[] = []
  const groups = new Map<string, LogGroupItem>()

  function groupFor(gid: string): LogGroupItem {
    const seen = groups.get(gid)
    if (seen) return seen
    const group: LogGroupItem = { kind: 'group', id: gid, header: null, members: [], summary: null }
    groups.set(gid, group)
    const cut = gid.lastIndexOf('/')
    if (cut === -1) items.push(group)
    else groupFor(gid.slice(0, cut)).members.push(group)
    return group
  }

  for (const entry of entries) {
    const gid = entry.group?.id
    if (!gid) {
      items.push({ kind: 'entry', entry })
      continue
    }
    const group = groupFor(gid)
    if (entry.group?.role === 'GroupHeader') group.header = entry
    else if (entry.group?.role === 'GroupSummary') group.summary = entry
    else group.members.push({ kind: 'entry', entry })
  }
  return dissolveBarless(items)
}

// A block with neither header nor summary has nothing to open, so its members stand in
// its place: an undo can leave a block's members behind without its header.
function dissolveBarless(items: LogItem[]): LogItem[] {
  return items.flatMap((item) => {
    if (item.kind !== 'group') return [item]
    item.members = dissolveBarless(item.members)
    return !item.header && !item.summary ? item.members : [item]
  })
}

/** An entry as plain text, for naming it in a confirmation. i18n parts give their
 * variables rather than being translated twice. */
export function logPartsText(parts: readonly LogPart[]): string {
  const text = (part: LogPart): string => {
    switch (part.tag) {
      case 'LogText':
        return part.contents
      case 'LogNumber':
        return String(part.contents)
      case 'LogDelta':
        return part.contents >= 0 ? `+${part.contents}` : String(part.contents)
      case 'LogRefPart':
        return part.contents.name
      case 'LogI18n':
        return Object.values(part.contents[1]).map(text).join(' ')
      case 'LogList':
        return part.contents.map(text).join(', ')
      case 'LogIcon':
        return ''
    }
  }
  return parts.map(text).join(' ').replace(/\s+/g, ' ').trim()
}
