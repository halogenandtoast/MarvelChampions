import { computed, nextTick, onScopeDispose, readonly, ref, shallowReadonly, shallowRef } from 'vue'
import { useI18n } from 'vue-i18n'
import { apiFetch, type DeckChoice } from '../api'
import { token } from '../auth'
import type { GameData, GameView, ServerMessage } from '../server'
import type { LogEntry } from '../gameLog'
import { useCards } from '../cards'
import type { RiverCard } from '../components/River.vue'
import { cancelHeldRiverSources, dealFrom, holdRiverSource, intoRiver, notePiles, riffle } from './dealing'
import { cancelViewTransitions, transitionsOn, withViewTransition } from './useViewTransition'

// how long a card turned up in the river is left to be read, and how quickly it is cleared
// for the next
const HOLD_MS = 650
const CLEAR_MS = 160
class AnimationCancelled extends Error {}

// entries are numbered from 1, so a page not reaching it has more before it
const olderThan = (page: LogEntry[]) => (page[0]?.seq ?? 1) > 1

const socketUrl = (path: string) => {
  const protocol = window.location.protocol === 'https:' ? 'wss' : 'ws'
  return `${protocol}://${window.location.host}${path}?token=${encodeURIComponent(token.value ?? '')}`
}

/** A game to play, or with `spectate` to watch: the same updates, read only. */
export const useGame = (gameId: string, spectate = false) => {
  const gamePath = `/api/games/${gameId}${spectate ? '/spectate' : ''}`
  const { t } = useI18n()
  let closed = false
  const error = ref<string | null>(null)
  const data = shallowRef<GameData | null>(null)
  const log = shallowRef<LogEntry[]>([])
  // cards the server said were dealt or turned up, shown in the river until the state after
  const river = shallowRef<RiverCard[]>([])
  // what the last state put in the river is over, cleared for what the server tells of next
  const riverCleared = ref(false)
  const canceledEvent = shallowRef<{ code: string; statuses: ('stunned' | 'confused')[] } | null>(null)
  const activationAlert = ref<string | null>(null)
  const activationBanner = ref(false)
  const activationKind = ref<'attack' | 'scheme' | 'playerPhase' | 'villainPhase'>('attack')
  const cards = useCards()
  const hasOlder = ref(false)
  const queue: ServerMessage[] = []
  let draining = false
  let socket: WebSocket | null = null
  let reconnect: number | undefined
  let socketUpdates = 0
  let animationEpoch = 0
  let animationController = new AbortController()
  let undoing = false
  const waitForAnimation = <T>(promise: Promise<T>): Promise<T> => {
    const signal = animationController.signal
    return new Promise((resolve, reject) => {
      const abort = () => reject(new AnimationCancelled())
      if (signal.aborted) { abort(); return }
      signal.addEventListener('abort', abort, { once: true })
      promise.then(value => { signal.removeEventListener('abort', abort); resolve(value) }, error => { signal.removeEventListener('abort', abort); reject(error) })
    })
  }
  const sleep = (ms: number) => {
    let timer: ReturnType<typeof setTimeout>
    return waitForAnimation(new Promise<void>(resolve => { timer = setTimeout(resolve, ms) }))
      .finally(() => clearTimeout(timer))
  }

  // the pile each encounter card dealt to a player lies on, until it is revealed from it
  const dealtTo = new Map<string, string>()

  const update = async (next: GameData, animate = true) => {
    const epoch = animationEpoch
    notePiles()
    for (const p of next.game?.players ?? []) for (const id of p.dealtCardIds ?? []) dealtTo.set(id, `dealt-${p.id}`)
    if (next.log) {
      log.value = next.log
      hasOlder.value = olderThan(next.log)
    }
    // cards morph between zones, as a hand card going into play, or fly from and to piles
    const before = data.value?.game ?? null
    const dealing = animate && transitionsOn() && next.game ? dealFrom(before, next.game, cards.sources, new Map(river.value.map((c) => [c.id, c.code]))) : null
    const applyState = async () => {
      if (epoch !== animationEpoch) return
      dealing?.lift()
      data.value = next
      river.value = []
      riverCleared.value = false
      canceledEvent.value = null
      await nextTick()
      if (epoch === animationEpoch) dealing?.land()
    }
    if (animate && before) await withViewTransition(applyState)
    else await applyState()
    dealing?.done()
  }

  // the river emptied, quickly, before the next card comes into it
  const clearRiver = async () => {
    const epoch = animationEpoch
    const g = data.value?.game
    const held = canceledEvent.value || river.value.length || (!riverCleared.value && (g?.focusedCards?.length || g?.boostCards?.length || g?.attack))
    if (!held) return
    const quick = Object.assign(document.createElement('style'), {
      textContent: `::view-transition-group(*), ::view-transition-old(*), ::view-transition-new(*) { animation-duration: ${CLEAR_MS}ms !important; }`,
    })
    document.head.append(quick)
    await withViewTransition(() => {
      if (epoch !== animationEpoch) return
      river.value = []
      riverCleared.value = true
      canceledEvent.value = null
    })
    quick.remove()
  }
  // a card into the river: a boost off the encounter deck face down, or a card turned up
  const toRiver = async (card: RiverCard, faceUp: boolean, flipInPlace = false) => {
    const epoch = animationEpoch
    if (flipInPlace) {
      river.value = [...river.value.filter(c => c.id !== card.id), card]
      await nextTick()
      if (!window.matchMedia('(prefers-reduced-motion: reduce)').matches) await sleep(560)
      await sleep(HOLD_MS)
      return
    }
    const moving = transitionsOn() ? intoRiver(card.id, faceUp, cards.sources, dealtTo.get(card.id), card.code) : null
    try {
      if (moving) await waitForAnimation(moving.ready)
      if (epoch !== animationEpoch) throw new AnimationCancelled()
      dealtTo.delete(card.id)
      await withViewTransition(() => {
        if (epoch !== animationEpoch) return
        moving?.lift()
        river.value = [...river.value.filter((c) => c.id !== card.id), card]
      })
      if (epoch !== animationEpoch) throw new AnimationCancelled()
    } finally { moving?.done() }
    if (faceUp && moving) await sleep(HOLD_MS)
  }

  // Log entries arrive while the server works; the state that follows is applied in order.
  const apply = async (message: ServerMessage) => {
    switch (message.tag) {
      case 'EventCanceled':
        await clearRiver()
        await withViewTransition(() => { canceledEvent.value = message })
        await sleep(1500)
        return
      case 'Update':
        return update(message.contents)
      case 'Logged':
        log.value = [...log.value, message.entry]
        for (const part of message.entry.body) {
          if (part.tag !== 'LogI18n') continue
          const [key, params] = part.contents
          if (key === 'log.phase.player' || key === 'log.phase.villain') {
            activationBanner.value = true
            activationKind.value = key === 'log.phase.player' ? 'playerPhase' : 'villainPhase'
            activationAlert.value = t(key)
            await nextTick()
            await sleep(2200)
            activationAlert.value = null
            continue
          }
          const enemy = params.enemy
          if (!['log.attacks', 'log.schemes'].includes(key) || enemy?.tag !== 'LogRefPart' || enemy.contents.kind !== 'RefVillain') continue
          const player = params.player
          activationBanner.value = true
          activationKind.value = key === 'log.attacks' ? 'attack' : 'scheme'
          activationAlert.value = t(key, { enemy: enemy.contents.name, player: player?.tag === 'LogRefPart' ? player.contents.name : '' })
          await nextTick()
          await sleep(2200)
          activationAlert.value = null
        }
        return
      case 'Shuffled':
        return transitionsOn() ? riffle(message.pile) : undefined
      case 'BoostDealt':
        return toRiver({ id: message.cardId, code: null, boost: true }, false)
      case 'Revealing':
        // an encounter card revealed has the river to itself; a boost turns up beside its fellows
        if (!message.boost) await clearRiver()
        return toRiver({ id: message.cardId, code: message.code, boost: message.boost }, true, message.boost)
      default:
        return
    }
  }

  const drain = async () => {
    if (draining) return
    draining = true
    try {
      while (queue.length) {
        try { await apply(queue.shift() as ServerMessage) }
        catch (error) { if (!(error instanceof AnimationCancelled)) throw error }
      }
    } finally { draining = false }
  }

  const post = async (path: string, body: unknown = {}) => {
    const response = await apiFetch(`/api/games/${gameId}${path}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    }).catch(() => null)
    error.value = response?.ok ? null : (await response?.text()) || t('game.errors.serverUnreachable')
    return response?.ok ?? false
  }

  const refresh = async () => {
    const epoch = animationEpoch
    if (undoing) return
    const updatesAtStart = socketUpdates
    const response = await apiFetch(gamePath).catch(() => null)
    if (response?.ok) {
      const contents = (await response.json()) as GameData
      // A broadcast received during this request is newer than its snapshot.
      // Applying the snapshot afterwards can rewind a deal and replay it.
      if (closed || epoch !== animationEpoch || socketUpdates !== updatesAtStart) return
      queue.push({ tag: 'Update', contents })
      void drain()
    } else if (response) {
      error.value = (await response.text()) || response.statusText
    }
  }

  const undoImmediately = async (path: string) => {
    if (undoing) return false
    undoing = true
    animationEpoch++
    animationController.abort()
    animationController = new AbortController()
    queue.length = 0
    activationAlert.value = null
    river.value = []
    riverCleared.value = true
    canceledEvent.value = null
    cancelHeldRiverSources()
    cancelViewTransitions()
    try {
      const accepted = await post(path)
      const response = await apiFetch(gamePath).catch(() => null)
      if (response?.ok && !closed) await update(await response.json() as GameData, false)
      cancelViewTransitions()
      return accepted
    } finally { undoing = false }
  }

  const connect = () => {
    socket = new WebSocket(socketUrl(gamePath))
    socket.addEventListener('open', () => {
      error.value = null
      void refresh()
    })
    socket.addEventListener('message', (event: MessageEvent<string>) => {
      if (undoing) return
      try {
        const message = JSON.parse(event.data) as ServerMessage
        if (message.tag === 'Update') socketUpdates++
        queue.push(message)
      } catch {
        return
      }
      void drain()
    })
    socket.addEventListener('close', () => {
      if (!closed) reconnect = window.setTimeout(connect, 2000)
    })
  }

  connect()

  onScopeDispose(() => {
    closed = true
    animationController.abort()
    window.clearTimeout(reconnect)
    socket?.close()
    cancelHeldRiverSources()
  })

  const game = computed<GameView | null>(() => data.value?.game ?? null)

  return {
    error: readonly(error),
    data: shallowReadonly(data),
    game,
    canUndo: computed(() => data.value?.canUndo ?? false),
    started: computed(() => data.value?.started ?? false),
    table: computed(() => data.value?.table ?? null),
    log: shallowReadonly(log),
    river: shallowReadonly(river),
    riverCleared: readonly(riverCleared),
    canceledEvent: readonly(canceledEvent),
    activationAlert: readonly(activationAlert),
    activationBanner: readonly(activationBanner),
    activationKind: readonly(activationKind),
    hasOlder: readonly(hasOlder),
    loadOlder: async (before: number) => {
      const response = await apiFetch(`/api/games/${gameId}/log?before=${before}`).catch(() => null)
      if (!response?.ok) return
      const older = (await response.json()) as LogEntry[]
      log.value = [...older, ...log.value]
      hasOlder.value = olderThan(older)
    },
    undo: (to?: 'phase' | 'round') => undoImmediately(to ? `/undo?to=${to}` : '/undo'),
    // back to just before a log entry
    undoTo: (step: number) => undoImmediately(`/undo?to=step&step=${step}`),
    say: (text: string) => post('/chat', { text }),
    reset: () => post('/reset'),
    // answered as the seat it was asked of
    answer: async (questionVersion: number, choice: number, player: string) => {
      const epoch = animationEpoch
      const question = data.value?.game?.question[player]
      const card = data.value?.game?.players.find(p => p.id === player)?.dealtCardIds?.[0]
      const held = card && transitionsOn() && question?.prompt === '$label.revealDealtEncounterCard'
        ? holdRiverSource(card, `dealt-${player}`, cards.sources) : null
      await held?.ready
      if (epoch !== animationEpoch) { held?.cancel(); return false }
      const accepted = await post('', { choice, questionVersion, player })
      if (!accepted) held?.cancel()
      return accepted
    },
    debug: (message: unknown) => post('/debug', message),
    join: (deck: DeckChoice, position?: number) => post('/join', { deck, position }),
    leave: () => post('/leave'),
    start: () => post('/start'),
    refresh,
  }
}
