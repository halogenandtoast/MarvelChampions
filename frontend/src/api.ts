import type { DeckList } from './browser/types/deck'
import { authHeaders, signOut } from './auth'
import { router } from './router'
import type { GameSummary, ScenarioList, StarterDeck } from './server'

/** A fetch that signs the request and sends a signed-out user to sign in. */
export const apiFetch = async (path: string, init: RequestInit = {}): Promise<Response> => {
  const response = await fetch(path, { ...init, headers: { ...authHeaders(), ...(init.headers ?? {}) } })
  if (response.status === 401) {
    signOut()
    await router.push({ path: '/sign-in', query: { next: router.currentRoute.value.fullPath } })
  }
  return response
}

const json = async <T>(response: Response): Promise<T> => {
  if (!response.ok) throw new Error((await response.text()) || response.statusText)
  return (await response.json()) as T
}

const post = (path: string, body: unknown) =>
  apiFetch(path, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })

export const fetchGames = () => apiFetch('/api/games').then((r) => json<GameSummary[]>(r))

/** Games in progress that are open to spectators. */
export const fetchSpectatableGames = () => apiFetch('/api/spectatable').then((r) => json<GameSummary[]>(r))

export const fetchScenarios = () => apiFetch('/api/scenarios').then((r) => json<ScenarioList>(r))

export const fetchStarterDecks = () => apiFetch('/api/starter-decks').then((r) => json<StarterDeck[]>(r))

export interface AdminDashboard {
  users: number
  activeUsers: number
  games: number
  recentGames: GameSummary[]
}

export const fetchAdminDashboard = () => apiFetch('/api/admin').then((r) => json<AdminDashboard>(r))

/** A core set starter by hero, one of the user's decks, or a list loaded for this game alone. */
export type DeckChoice = { starter: string } | { deckId: number } | { list: DeckList }

export interface NewGame {
  name: string
  seed?: number
  scenario?: { scenarioCode: string; modulars: string[]; difficulty: 'Normal' | 'Expert' }
  decks: DeckChoice[]
  seats?: number
  allowSpectators?: boolean
}

export const createGame = (game: NewGame) => post('/api/games', game).then((r) => json<{ id: string }>(r))

export const deleteGame = (id: string) => apiFetch(`/api/games/${id}`, { method: 'DELETE' })

/** Download a game as a JSON file. */
export const exportGame = async (id: string) => {
  const response = await apiFetch(`/api/games/${id}/export`)
  if (!response.ok) throw new Error((await response.text()) || response.statusText)
  const filename = /filename="([^"]+)"/.exec(response.headers.get('Content-Disposition') ?? '')?.[1] ?? `game-${id}.json`
  const url = URL.createObjectURL(await response.blob())
  const link = Object.assign(document.createElement('a'), { href: url, download: filename })
  link.click()
  URL.revokeObjectURL(url)
}

/** Upload a game's export for a bug report, giving back where it went. */
export const fileBug = (id: string) => apiFetch(`/api/games/${id}/file-bug`, { method: 'POST' }).then((r) => json<{ url: string }>(r))

/** Load an exported game file as a new game. */
export const importGame = async (file: File) =>
  apiFetch('/api/import', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: await file.text(),
  }).then((r) => json<{ id: string }>(r))
