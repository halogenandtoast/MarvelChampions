import { computed, ref } from 'vue'

export interface User {
  id: number
  username: string
  email: string
  admin: boolean
  beta: boolean
  allowSpectatorsByDefault: boolean
  guest: boolean
}

interface Session {
  token: string
  user: User
}

const STORAGE_KEY = 'marvel-session'

const load = (): Session | null => {
  try {
    const stored = localStorage.getItem(STORAGE_KEY)
    return stored ? (JSON.parse(stored) as Session) : null
  } catch {
    return null
  }
}

const session = ref<Session | null>(load())

export const currentUser = computed(() => session.value?.user ?? null)
export const token = computed(() => session.value?.token ?? null)

const save = (next: Session | null) => {
  session.value = next
  try {
    if (next) localStorage.setItem(STORAGE_KEY, JSON.stringify(next))
    else localStorage.removeItem(STORAGE_KEY)
  } catch {
    // storage unavailable: the session lasts until the page closes
  }
}

export const authHeaders = (): Record<string, string> =>
  token.value ? { Authorization: `Token ${token.value}` } : {}

const post = async (path: string, body: unknown) => {
  const response = await fetch(path, {
    method: 'POST',
    // a guest signing up keeps their games
    headers: { ...authHeaders(), 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  })
  if (!response.ok) throw new Error((await response.text()) || response.statusText)
  save((await response.json()) as Session)
}

export const signIn = (login: string, password: string) => post('/api/authenticate', { login, password })

export const signUp = (username: string, email: string, password: string) =>
  post('/api/register', { username, email, password })

export const playAsGuest = () => post('/api/guest', {})

export const signOut = () => save(null)

/** Save the user's settings, keeping the stored user in step. */
export const updateSettings = async (settings: { allowSpectatorsByDefault: boolean }) => {
  const response = await fetch('/api/settings', {
    method: 'PUT',
    headers: { ...authHeaders(), 'Content-Type': 'application/json' },
    body: JSON.stringify(settings),
  })
  if (!response.ok) throw new Error((await response.text()) || response.statusText)
  if (session.value) save({ ...session.value, user: (await response.json()) as User })
}

/** Bring the stored user up to date, such as when they have been made an admin. */
export const refreshUser = async () => {
  if (!session.value) return
  const response = await fetch('/api/whoami', { headers: authHeaders() }).catch(() => null)
  // a guest away long enough is swept
  if (response?.status === 401) return save(null)
  if (response?.ok && session.value) save({ ...session.value, user: (await response.json()) as User })
}
