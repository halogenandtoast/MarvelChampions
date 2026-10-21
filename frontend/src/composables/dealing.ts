// Cards flying between the board and its piles. The client sees only each new state, so it
// works out what moved by comparing it with the last, and lays a stand-in over a pile for a
// card that has nothing on screen at one end; the view transition morphs one into the other.
import { BACKS } from '../cards'
import type { GameView } from '../server'

const DEAL_MS = 520
const STAGGER_MS = 90
const RIFFLE_MS = 900
const RIFFLE_CARDS = 10

/** The view-transition name a card moves under, as BoardCard gives it. */
export const morphName = (id: string) => `c${id.replace(/[^a-zA-Z0-9-]/g, '')}`

export type Sources = (code: string) => string[]

interface Move {
  id: string
  pile: string
  /** a card back flying, turned up as it lands */
  faceDown: boolean
  ghost: HTMLElement
}

const onScreen = (id: string) => document.querySelector(`[data-card-id="${CSS.escape(id)}"]`) !== null
const pileAt = (pile: string) => document.querySelector<HTMLElement>(`[data-pile="${pile}"] .stack`)

const ghostFor = (id: string, pile: string, code: string | null, sources: Sources): Move => {
  const faceDown = code === null
  const ghost = Object.assign(document.createElement('img'), {
    src: faceDown ? BACKS[pile.endsWith('-encounter') ? 'encounter' : 'player'] : (sources(code)[0] ?? ''),
    alt: '',
  })
  ghost.style.borderRadius = '7px'
  ghost.style.setProperty('view-transition-name', morphName(id))
  return { id, pile, faceDown, ghost }
}

const lay = ({ pile, ghost }: Move) => {
  const at = pileAt(pile)
  if (!at) return false
  const { left, top, width, height } = at.getBoundingClientRect()
  Object.assign(ghost.style, { position: 'fixed', left: `${left}px`, top: `${top}px`, width: `${width}px`, height: `${height}px`, margin: '0', pointerEvents: 'none' })
  document.body.append(ghost)
  return true
}

// a card turned face up as it flies, or where it lies
const turnRules = (name: string, delay: number) => [
  `::view-transition-image-pair(${name}) { perspective: 800px; }`,
  `::view-transition-old(${name}) { animation: deal-turn-away ${DEAL_MS}ms ease-in ${delay}ms both; }`,
  `::view-transition-new(${name}) { animation: deal-turn-up ${DEAL_MS}ms ease-out ${delay}ms both; }`,
]
const flyRules = (name: string, delay: number) => [
  `::view-transition-group(${name}) { animation-duration: ${DEAL_MS}ms; animation-delay: ${delay}ms; animation-fill-mode: both; z-index: 1; }`,
  `::view-transition-old(${name}), ::view-transition-new(${name}) { animation-delay: ${delay}ms; animation-fill-mode: both; }`,
]

const styled = (rules: string[]) => {
  const style = Object.assign(document.createElement('style'), { textContent: rules.join('\n') })
  document.head.append(style)
  return style
}

// Where each card on the board is drawn from: a player's own cards from their deck, the
// rest from the encounter deck.
const ownerOf = (view: GameView) => {
  const owner = new Map<string, string>()
  for (const p of view.players ?? []) {
    const add = (xs: readonly { id: string }[] | null | undefined) => xs?.forEach((x) => owner.set(x.id, p.id))
    add(p.hand)
    add(p.allies)
    p.allies?.forEach((a) => add(a.upgrades))
    add(p.supports)
    add(p.upgrades)
    add(p.discard)
  }
  return owner
}

// the cards a state shows on the table or in hand, with their faces
const shown = (view: GameView) => {
  const cards = new Map<string, string | null>()
  const add = (xs: readonly { id: string; code: string | null }[] | null | undefined) => xs?.forEach((x) => cards.set(x.id, x.code))
  for (const p of view.players ?? []) {
    add(p.hand)
    add(p.allies)
    p.allies?.forEach((a) => add(a.upgrades))
    add(p.supports)
    add(p.upgrades)
  }
  for (const v of view.villains ?? []) {
    add(v.attachments)
    add(v.upgrades)
  }
  for (const m of view.minions ?? []) {
    cards.set(m.id, m.code)
    add(m.attachments)
    add(m.upgrades)
  }
  add(view.sideSchemes)
  add(view.environments)
  add(view.focusedCards)
  add(view.boostCards)
  if (view.activeCost) {
    add(view.activeCost.card ? [view.activeCost.card] : [])
    add(view.activeCost.spent)
  }
  return cards
}

const discardsOf = (view: GameView) => {
  const pile = new Map<string, string>()
  for (const c of view.encounterDiscard ?? []) pile.set(c.id, 'discard-encounter')
  for (const p of view.players ?? []) for (const c of p.discard ?? []) pile.set(c.id, `discard-${p.id}`)
  return pile
}

/**
 * The moves from one state to the next. `lift` goes inside the transition before the new
 * state is applied, `land` after it renders, `done` once the transition is over.
 */
export const dealFrom = (before: GameView | null, after: GameView, sources: Sources, extra: ReadonlyMap<string, string | null> = new Map()) => {
  if (!before) return null
  const leaving: Move[] = []
  const arriving: Move[] = []
  const wasDiscarded = discardsOf(before)
  const isDiscarded = discardsOf(after)
  const owner = ownerOf(after)
  const next = shown(after)

  // new on the table: from a discard pile it was in, or the top of a deck
  for (const [id, code] of next) {
    if (onScreen(id)) continue
    const from = wasDiscarded.get(id)
    if (from) leaving.push(ghostFor(id, from, code, sources))
    else {
      const pid = owner.get(id)
      const move = ghostFor(id, pid ? `deck-${pid}` : 'deck-encounter', null, sources)
      // a boost dealt face down stays face down
      move.faceDown = code !== null
      leaving.push(move)
    }
  }
  // gone from the table into a discard pile
  // with what the river held that the last state did not show
  const was = new Map([...shown(before), ...extra])
  for (const [id, code] of was) {
    if (next.has(id) || !onScreen(id)) continue
    const to = isDiscarded.get(id)
    if (to && !wasDiscarded.has(id)) arriving.push(ghostFor(id, to, code, sources))
  }
  if (!leaving.length && !arriving.length) return null

  const rules: string[] = []
  arriving.forEach((m, i) => rules.push(...flyRules(morphName(m.id), i * STAGGER_MS)))
  const dealAt = arriving.length ? STAGGER_MS * 2 : 0
  leaving.forEach((m, i) => {
    const delay = dealAt + i * STAGGER_MS
    rules.push(...flyRules(morphName(m.id), delay))
    if (m.faceDown) rules.push(...turnRules(morphName(m.id), delay))
  })
  leaving.forEach(lay)
  const style = styled(rules)
  return {
    lift: () => leaving.forEach((m) => m.ghost.remove()),
    land: () => arriving.forEach(lay),
    done: () => {
      arriving.forEach((m) => m.ghost.remove())
      style.remove()
    },
  }
}

/** A card coming into the river: off the encounter deck, or turned up where it lies. */
export const intoRiver = (id: string, faceUp: boolean, sources: Sources) => {
  const name = morphName(id)
  const rules = faceUp ? turnRules(name, 0) : []
  let ghost: Move | null = null
  if (!onScreen(id)) {
    ghost = ghostFor(id, 'deck-encounter', null, sources)
    if (!lay(ghost)) ghost = null
    rules.push(...flyRules(name, 0))
  }
  const style = styled(rules)
  return {
    lift: () => ghost?.ghost.remove(),
    done: () => style.remove(),
  }
}

/** A deck riffled: its top cards split in two and fall back together. */
export const riffle = (pile: string): Promise<void> => {
  const deck = pileAt(pile)
  if (!deck || deck.closest('.empty') || window.matchMedia('(prefers-reduced-motion: reduce)').matches) return Promise.resolve()
  const face = deck.querySelector('img')
  const { left, top, width, height } = deck.getBoundingClientRect()
  const half = RIFFLE_CARDS / 2
  deck.style.visibility = 'hidden'
  const cards = Array.from({ length: RIFFLE_CARDS }, (_, n) => {
    const side = n < half ? -1 : 1
    const level = n % half
    const rank = level * 2 + (side < 0 ? 0 : 1)
    const card = Object.assign(document.createElement('img'), { src: face?.currentSrc || face?.src || '', alt: '' })
    Object.assign(card.style, {
      position: 'fixed', left: `${left}px`, top: `${top}px`, width: `${width}px`, height: `${height}px`,
      margin: '0', borderRadius: '7px', zIndex: `${1000 + rank}`, boxShadow: '0 1px 3px rgb(0 0 0 / 0.5)', pointerEvents: 'none',
    })
    document.body.append(card)
    const squared = `translate(0, ${-rank * 0.6}px)`
    const apart = `translate(${side * width * 0.42}px, ${-8 - level * 1.2}px) rotate(${side * 6}deg)`
    const drops = 0.32 + rank * 0.045
    return card.animate(
      [
        { offset: 0, transform: `translate(0, ${-n * 0.6}px)`, easing: 'ease-out' },
        { offset: 0.24, transform: apart, easing: 'linear' },
        { offset: drops, transform: apart, easing: 'cubic-bezier(0.5, 0, 0.75, 0)' },
        { offset: drops + 0.1, transform: squared, easing: 'linear' },
        { offset: 0.92, transform: squared, easing: 'ease-in-out' },
        { offset: 1, transform: 'none' },
      ],
      { duration: RIFFLE_MS, fill: 'both' },
    )
  })
  return Promise.all(cards.map((a) => a.finished.catch(() => undefined))).then(() => {
    cards.forEach((a) => (a.effect as KeyframeEffect).target?.remove())
    deck.style.removeProperty('visibility')
  })
}
