import { computed, onScopeDispose, readonly, ref, shallowReadonly, shallowRef } from 'vue'
import { useI18n } from 'vue-i18n'
import { apiFetch, type DeckChoice } from '../api'
import { token } from '../auth'
import type { GameData, GameView, ServerMessage } from '../server'
import type { LogEntry } from '../gameLog'
import { withViewTransition } from './useViewTransition'

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
  const hasOlder = ref(false)
  const queue: ServerMessage[] = []
  let draining = false
  let socket: WebSocket | null = null
  let reconnect: number | undefined

  const update = async (next: GameData) => {
    if (next.log) {
      log.value = next.log
      hasOlder.value = olderThan(next.log)
    }
    // cards morph between zones, as a hand card going into play
    await withViewTransition(() => {
      data.value = next
    })
  }

  // Log entries arrive while the server works; the state that follows is applied in order.
  const apply = async (message: ServerMessage) => {
    switch (message.tag) {
      case 'Update':
        return update(message.contents)
      case 'Logged':
        log.value = [...log.value, message.entry]
        return
      default:
        return
    }
  }

  const drain = async () => {
    if (draining) return
    draining = true
    while (queue.length) await apply(queue.shift() as ServerMessage)
    draining = false
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
    const response = await apiFetch(gamePath).catch(() => null)
    if (response?.ok) {
      queue.push({ tag: 'Update', contents: (await response.json()) as GameData })
      void drain()
    } else if (response) {
      error.value = (await response.text()) || response.statusText
    }
  }

  const connect = () => {
    socket = new WebSocket(socketUrl(gamePath))
    socket.addEventListener('open', () => {
      error.value = null
      void refresh()
    })
    socket.addEventListener('message', (event: MessageEvent<string>) => {
      try {
        queue.push(JSON.parse(event.data) as ServerMessage)
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
    window.clearTimeout(reconnect)
    socket?.close()
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
    hasOlder: readonly(hasOlder),
    loadOlder: async (before: number) => {
      const response = await apiFetch(`/api/games/${gameId}/log?before=${before}`).catch(() => null)
      if (!response?.ok) return
      const older = (await response.json()) as LogEntry[]
      log.value = [...older, ...log.value]
      hasOlder.value = olderThan(older)
    },
    undo: (to?: 'phase' | 'round') => post(to ? `/undo?to=${to}` : '/undo'),
    // back to just before a log entry
    undoTo: (step: number) => post(`/undo?to=step&step=${step}`),
    say: (text: string) => post('/chat', { text }),
    reset: () => post('/reset'),
    // answered as the seat it was asked of
    answer: (questionVersion: number, choice: number, player: string) => post('', { choice, questionVersion, player }),
    debug: (message: unknown) => post('/debug', message),
    join: (deck: DeckChoice) => post('/join', { deck }),
    leave: () => post('/leave'),
    start: () => post('/start'),
    refresh,
  }
}
