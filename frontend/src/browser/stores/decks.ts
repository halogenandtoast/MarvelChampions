/**
 * A user's decks.
 *
 * The list is cached because it is small and the deck page links back to it;
 * a single deck is not, because importing one is the only thing that changes
 * it and that goes through here too.
 */
import { computed, ref, watch } from 'vue'
import { defineStore } from './store'
import { currentUser } from '../../auth'
import * as api from '@browser/lib/api'
import type { Deck, DeckSummary, ImportRequest } from '@browser/types/deck'

export type LoadState = 'idle' | 'loading' | 'ready' | 'error'

export const useDeckStore = defineStore('decks', () => {
  const decks = ref<DeckSummary[]>([])
  const state = ref<LoadState>('idle')
  const error = ref<string | null>(null)

  const isEmpty = computed(() => state.value === 'ready' && decks.value.length === 0)

  // forced, it refreshes the list it has in place, without showing it loading again
  async function load(force = false): Promise<void> {
    if (state.value === 'ready' && !force) return
    if (state.value !== 'ready') state.value = 'loading'
    error.value = null
    try {
      decks.value = await api.fetchDecks()
      state.value = 'ready'
    } catch (err) {
      error.value = messageOf(err)
      state.value = 'error'
    }
  }

  /** Import a deck and add it to the list. Throws for the caller to show. */
  async function importDeck(request: ImportRequest): Promise<Deck> {
    const deck = await api.importDeck(request)
    await load(true)
    return deck
  }

  async function remove(id: number): Promise<void> {
    await api.deleteDeck(id)
    decks.value = decks.value.filter((deck) => deck.id !== id)
  }

  /** Drop everything on sign-out, so the next user does not see the last one's decks. */
  function reset(): void {
    decks.value = []
    state.value = 'idle'
    error.value = null
  }

  // a different account must not see the last one's decks
  watch(() => currentUser.value?.id, reset)

  return { decks, state, error, isEmpty, load, importDeck, remove, reset }
})

export function messageOf(err: unknown): string {
  return err instanceof Error ? err.message : String(err)
}
