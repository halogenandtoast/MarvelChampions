// Cards flying between the board and its piles. The client sees only each new state, so it
// works out what moved by comparing it with the last, and lays a stand-in over a pile for a
// card that has nothing on screen at one end; the view transition morphs one into the other.
import { BACKS, useCards } from '../cards'
import type { GameView } from '../server'
import { searchCardIds } from './cardSearch'

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
  /** The unrotated landscape card before it leaves the board. */
  from?: DOMRect
}

const heldRiverSources = new Map<string, Move>()
const onScreen = (id: string) => heldRiverSources.has(id) || document.querySelector(`[data-card-id="${CSS.escape(id)}"]`) !== null
const pileAt = (pile: string) => document.querySelector<HTMLElement>(`[data-pile="${pile}"] .stack`)

// where each pile last was, for a card leaving one that is gone once it is empty, as the
// last encounter card dealt to a player is when it is revealed
const lastSeen = new Map<string, DOMRect>()
export const notePiles = () =>
  document.querySelectorAll<HTMLElement>('[data-pile]').forEach((el) => {
    const stack = el.querySelector('.stack')
    if (stack && el.dataset.pile) lastSeen.set(el.dataset.pile, stack.getBoundingClientRect())
  })
// a player's encounter cards are dealt from the encounter deck, and show its back
const encounterBack = (pile: string) => pile.endsWith('-encounter') || pile.startsWith('dealt-')

const ghostFor = (id: string, pile: string, code: string | null, sources: Sources): Move => {
  const faceDown = code === null
  const image = Object.assign(document.createElement('img'), {
    src: faceDown ? BACKS[encounterBack(pile) ? 'encounter' : 'player'] : (sources(code)[0] ?? ''),
    alt: '',
  })
  const ghost = !faceDown && useCards().landscape(code) ? document.createElement('div') : image
  if (ghost !== image) {
    ghost.append(image)
    Object.assign(image.style, { position: 'absolute', left: '50%', top: '50%', transform: 'translate(-50%, -50%) rotate(90deg)', borderRadius: 'inherit', objectFit: 'cover' })
  }
  ghost.style.borderRadius = '7px'
  ghost.style.setProperty('view-transition-name', morphName(id))
  return { id, pile, faceDown, ghost }
}

// a card put beneath a scheme lands on it, card sized: `card:` and the scheme's id
const cardAt = (id: string) => {
  const r = document.querySelector(`[data-card-id="${CSS.escape(id)}"]`)?.getBoundingClientRect()
  if (!r) return undefined
  const height = Math.min(r.height, r.width / 0.716)
  const width = height * 0.716
  return { left: r.left + (r.width - width) / 2, top: r.top + (r.height - height) / 2, width, height }
}

const lay = ({ pile, ghost }: Move) => {
  const rect = pile.startsWith('card:') ? cardAt(pile.slice(5)) : (pileAt(pile)?.getBoundingClientRect() ?? lastSeen.get(pile))
  if (!rect) return false
  const { left, top, width, height } = rect
  Object.assign(ghost.style, { position: 'fixed', left: `${left}px`, top: `${top}px`, width: `${width}px`, height: `${height}px`, margin: '0', pointerEvents: 'none', zIndex: '1000' })
  const image = ghost.querySelector('img')
  if (image) Object.assign(image.style, { width: `${height}px`, height: `${width}px` })
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

// Keep a landscape snapshot rigid as it travels into the portrait discard stack.
// The destination is measured after rendering, so layout changes are included.
const rotateToDiscard = (move: Move, delay: number): string[] => {
  const from = move.from
  if (!from || !move.ghost.isConnected) return []
  const to = move.ghost.getBoundingClientRect()
  const name = morphName(move.id)
  const pose = (x: number, y: number, angle: number, sx: number, sy: number) =>
    `translate(${x}px, ${y}px) rotate(${angle}deg) scale(${sx}, ${sy}) translate(${-from.width / 2}px, ${-from.height / 2}px)`
  return [
    `@keyframes discard-spin-${name} { from { transform: ${pose(from.left + from.width / 2, from.top + from.height / 2, 0, 1, 1)}; } to { transform: ${pose(to.left + to.width / 2, to.top + to.height / 2, 90, to.height / from.width, to.width / from.height)}; } }`,
    `::view-transition-group(${name}) { width: ${from.width}px !important; height: ${from.height}px !important; transform-origin: 0 0; animation: discard-spin-${name} ${DEAL_MS}ms ease-in-out ${delay}ms both; }`,
    `::view-transition-old(${name}) { animation: none; opacity: 1; width: 100%; height: 100%; mix-blend-mode: normal; }`,
    `::view-transition-new(${name}) { animation: none; opacity: 0; }`,
  ]
}

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
  const searching = searchCardIds(view)
  add(view.focusedCards?.filter((card) => !searching.has(card.id)))
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
  for (const c of view.victoryDisplay ?? []) pile.set(c.id, 'victory-display')
  for (const c of view.collection ?? []) pile.set(c.id, 'collection')
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
  const was = new Map([...shown(before), ...extra])

  // new on the table: from a discard pile it was in, or the top of a deck
  for (const [id, code] of next) {
    // Other seats' hands and collapsed zones need not have a DOM card. Their
    // absence on screen does not mean the server just dealt them again.
    if (was.has(id) || onScreen(id)) continue
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
  for (const [id, code] of was) {
    if (next.has(id) || !onScreen(id)) continue
    const to = isDiscarded.get(id)
    if (to && !wasDiscarded.has(id)) {
      const move = ghostFor(id, to, code, sources)
      if (code && to.startsWith('discard-') && useCards().landscape(code)) {
        const bounds = document.querySelector(`[data-card-id="${CSS.escape(id)}"]`)?.getBoundingClientRect()
        if (bounds && bounds.width > bounds.height) move.from = bounds
      }
      arriving.push(move)
    }
  }
  // dealt face down to a player: off the encounter deck onto their dealt pile
  const wasDealt = new Set((before.players ?? []).flatMap((p) => p.dealtCardIds ?? []))
  for (const p of after.players ?? [])
    for (const id of p.dealtCardIds ?? []) {
      if (wasDealt.has(id)) continue
      const dealt = ghostFor(id, 'deck-encounter', null, sources)
      dealt.faceDown = false
      leaving.push(dealt)
      arriving.push(ghostFor(id, `dealt-${p.id}`, null, sources))
    }
  // put facedown beneath a scheme: from the hand card it was, or else its owner's discard
  // pile or deck, onto the scheme, where it is gone
  const heldBy = (view: GameView) =>
    new Map([...(view.mainSchemes ?? []), ...(view.sideSchemes ?? [])].flatMap((s) => (s.heldCards ?? []).map((h) => [h.id, { scheme: s.id, owner: h.owner }] as const)))
  const wasHeld = heldBy(before)
  for (const [id, { scheme, owner }] of heldBy(after)) {
    if (wasHeld.has(id)) continue
    if (!onScreen(id)) {
      const from = ghostFor(id, wasDiscarded.get(id) ?? (owner ? `deck-${owner}` : 'deck-encounter'), null, sources)
      from.faceDown = false
      leaving.push(from)
    }
    arriving.push(ghostFor(id, `card:${scheme}`, null, sources))
  }
  if (!leaving.length && !arriving.length) return null

  const rules: string[] = []
  // a card dealt to a player both leaves and arrives: it flies once, with the cards dealt
  const both = new Set(leaving.map((m) => m.id))
  const discarding = arriving.filter((m) => !both.has(m.id))
  discarding.forEach((m, i) => rules.push(...flyRules(morphName(m.id), i * STAGGER_MS)))
  const dealAt = discarding.length ? STAGGER_MS * 2 : 0
  leaving.forEach((m, i) => {
    const delay = dealAt + i * STAGGER_MS
    rules.push(...flyRules(morphName(m.id), delay))
    if (m.faceDown) rules.push(...turnRules(morphName(m.id), delay))
  })
  leaving.forEach(lay)
  const style = styled(rules)
  return {
    lift: () => leaving.forEach((m) => m.ghost.remove()),
    land: () => {
      arriving.forEach(lay)
      style.textContent += '\n' + discarding.flatMap((move, i) => rotateToDiscard(move, i * STAGGER_MS)).join('\n')
    },
    done: () => {
      leaving.forEach((m) => m.ghost.remove())
      arriving.forEach((m) => m.ghost.remove())
      style.remove()
    },
  }
}

/** A card coming into the river: off a pile (the encounter deck, or the cards dealt a
 * player), or turned up where it lies. */
/** Keep the clicked card visible even if the next server state removes its pile. */
export const holdRiverSource = (id: string, from: string, sources: Sources) => {
  const previous = heldRiverSources.get(id)
  if (previous) return { ready: Promise.resolve(), cancel: () => { previous.ghost.remove(); heldRiverSources.delete(id) } }
  const move = ghostFor(id, from, null, sources)
  move.ghost.dataset.riverSource = id
  move.ghost.style.viewTransitionName = 'none'
  if (!lay(move)) return null
  heldRiverSources.set(id, move)
  return {
    ready: (move.ghost as HTMLImageElement).decode().catch(() => undefined),
    cancel: () => { move.ghost.remove(); heldRiverSources.delete(id) },
  }
}

export const cancelHeldRiverSources = () => {
  for (const move of heldRiverSources.values()) move.ghost.remove()
  heldRiverSources.clear()
}

export const intoRiver = (id: string, faceUp: boolean, sources: Sources, from = 'deck-encounter', revealedCode: string | null = null) => {
  const name = morphName(id)
  const rules = faceUp ? turnRules(name, 0) : []
  let ghost: Move | null = heldRiverSources.get(id) ?? null
  heldRiverSources.delete(id)
  if (ghost) {
    ghost.ghost.style.viewTransitionName = name
    rules.push(...flyRules(name, 0))
  } else if (!onScreen(id)) {
    ghost = ghostFor(id, from, null, sources)
    if (!lay(ghost)) ghost = null
    rules.push(...flyRules(name, 0))
  }
  const style = styled(rules)
  // Both snapshots need decoded pixels before the transition starts. A freshly
  // inserted image can otherwise produce an empty source or destination frame.
  const images: HTMLImageElement[] = ghost
    ? ghost.ghost instanceof HTMLImageElement ? [ghost.ghost] : [...ghost.ghost.querySelectorAll('img')]
    : []
  if (faceUp && revealedCode) {
    const face = new Image()
    face.src = sources(revealedCode)[0] ?? ''
    images.push(face)
  }
  return {
    ready: Promise.all(images.map(image => image.decode().catch(() => undefined))),
    lift: () => ghost?.ghost.remove(),
    done: () => { ghost?.ghost.remove(); style.remove() },
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
