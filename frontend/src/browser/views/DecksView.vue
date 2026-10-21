<script setup lang="ts">
/**
 * A user's decks, and the row to load another above them, found by name, hero or aspect
 * and sorted, by default, as they were last played.
 */
import '@browser/browser.css'
import { computed, onMounted, ref, useTemplateRef } from 'vue'
import { useRouter } from 'vue-router'
import { TriangleAlert, Plus, Trash2 } from 'lucide-vue-next'
import DeckImport from '@browser/components/DeckImport.vue'
import ConfirmDelete from '../../components/ConfirmDelete.vue'
import HeroPortrait from '../../components/HeroPortrait.vue'
import { useDeckStore, messageOf } from '@browser/stores/decks'
import { ASPECTS } from '@browser/types/card'
import type { Deck, DeckSummary } from '@browser/types/deck'
import { useServerText } from '../../i18n'

const tr = useServerText()
const router = useRouter()
const decks = useDeckStore()

const removing = ref<number | null>(null)
const removeError = ref<string | null>(null)

// afresh each visit: a deck saved from a lobby, or one just played, changes the list
onMounted(() => decks.load(true))

const onImported = (deck: Deck) => void router.push({ name: 'deck', params: { id: deck.id } })

// asked first, as deleting a game is
const confirm = useTemplateRef<InstanceType<typeof ConfirmDelete>>('confirm')
const deleting = ref<DeckSummary | null>(null)
const ask = (deck: DeckSummary) => {
  deleting.value = deck
  confirm.value?.open()
}
async function remove(deck: DeckSummary): Promise<void> {
  if (removing.value !== null) return
  removing.value = deck.id
  removeError.value = null
  try {
    await decks.remove(deck.id)
  } catch (err) {
    removeError.value = messageOf(err)
  } finally {
    removing.value = null
  }
}

// the loading row waits behind New deck; open from the start with no decks
const loadOpen = ref<boolean | null>(null)
const showLoad = computed(() => loadOpen.value ?? decks.isEmpty)

const search = ref('')
const aspects = ref<string[]>([])
const SORTS = ['used', 'name', 'hero', 'imported'] as const
const sort = ref<(typeof SORTS)[number]>('used')
const reversed = ref(false)
const toggleAspect = (aspect: string) => {
  aspects.value = aspects.value.includes(aspect) ? aspects.value.filter((a) => a !== aspect) : [...aspects.value, aspect]
}

const time = (at: string | null) => (at ? Date.parse(at) : 0)
const compare: Record<(typeof SORTS)[number], (a: DeckSummary, b: DeckSummary) => number> = {
  used: (a, b) => time(b.usedAt ?? b.updatedAt) - time(a.usedAt ?? a.updatedAt),
  name: (a, b) => a.name.localeCompare(b.name),
  hero: (a, b) => a.hero.name.localeCompare(b.hero.name) || a.name.localeCompare(b.name),
  imported: (a, b) => time(b.updatedAt) - time(a.updatedAt),
}
const shown = computed(() => {
  const words = search.value.trim().toLowerCase()
  const matching = decks.decks.filter(
    (d) => (!words || [d.name, d.hero.name].some((n) => n.toLowerCase().includes(words))) && aspects.value.every((a) => d.aspects.includes(a)),
  )
  const sorted = [...matching].sort(compare[sort.value])
  return reversed.value ? sorted.reverse() : sorted
})
</script>

<template>
  <div class="page-container">
    <main class="page-content card-browser">
      <header class="head">
        <h1 class="page-title">{{ $t('browser.decks.title') }}</h1>
        <p v-if="decks.decks.length" class="muted">{{ $t('browser.decks.count', decks.decks.length) }}</p>
        <button v-if="showLoad" type="button" class="btn ghost new-deck" @click="loadOpen = false">{{ $t('common.cancel') }}</button>
        <button v-else type="button" class="btn primary new-deck" @click="loadOpen = true"><Plus aria-hidden="true" />{{ $t('browser.decks.new') }}</button>
      </header>

      <Transition name="fade">
        <div v-if="showLoad" class="import">
          <p class="muted">{{ $t('browser.decks.importHint') }}</p>
          <DeckImport @imported="onImported" />
        </div>
      </Transition>

      <p v-if="decks.state === 'loading'" class="notice">{{ $t('browser.common.loading') }}</p>
      <p v-else-if="decks.state === 'error'" class="notice notice--error">{{ tr(decks.error) }}</p>
      <p v-else-if="decks.isEmpty" class="notice">{{ $t('browser.decks.empty') }}</p>

      <template v-else>
        <div class="toolbar">
          <input v-model="search" class="input search" type="search" :placeholder="$t('browser.decks.search')" :aria-label="$t('browser.decks.search')" autocomplete="off" />
          <div class="aspects" role="group" :aria-label="$t('browser.decks.aspects')">
            <button
              v-for="aspect in ASPECTS"
              :key="aspect"
              type="button"
              class="aspect-toggle"
              :data-aspect="aspect"
              :aria-pressed="aspects.includes(aspect)"
              @click="toggleAspect(aspect)"
            >
              {{ $t(`aspects.${aspect}`) }}
            </button>
          </div>
          <label>
            <span class="visually-hidden">{{ $t('browser.browse.sortBy') }}</span>
            <select v-model="sort" class="input">
              <option v-for="value in SORTS" :key="value" :value="value">{{ $t(`browser.decks.sort.${value}`) }}</option>
            </select>
          </label>
          <button type="button" class="button button--ghost" :aria-pressed="reversed" :title="$t('browser.browse.reverseSort')" @click="reversed = !reversed">
            <span aria-hidden="true">{{ reversed ? '↑' : '↓' }}</span>
            <span class="visually-hidden">{{ $t('browser.browse.reverseSort') }}</span>
          </button>
        </div>

        <p v-if="!shown.length" class="notice">{{ $t('browser.decks.noneMatch') }}</p>
        <ul v-else class="decks">
          <li v-for="deck in shown" :key="deck.id" class="deck">
            <RouterLink class="deck__link" :to="{ name: 'deck', params: { id: deck.id } }">
              <HeroPortrait class="deck__art" :code="deck.hero.code" :aspects="deck.aspects" />
              <div class="deck__body">
                <h2 class="deck__name">{{ deck.name }}</h2>
                <p class="deck__hero">{{ deck.hero.name }}</p>
                <p class="deck__meta">
                  <span v-for="aspect in deck.aspects" :key="aspect" class="aspect-chip" :data-aspect="aspect">{{ $t(`aspects.${aspect}`) }}</span>
                  <span>{{ $t('browser.common.cardCount', deck.cardCount) }}</span>
                  <span v-if="deck.source === 'marvelcdb'" class="chip">MarvelCDB</span>
                  <span v-if="deck.problem" class="chip chip--warn" :title="tr(deck.problem)"><TriangleAlert aria-hidden="true" />{{ $t('browser.decks.unplayable') }}</span>
                </p>
              </div>
            </RouterLink>
            <button type="button" class="deck__remove" :disabled="removing === deck.id" :title="$t('browser.decks.delete', { name: deck.name })" @click="ask(deck)">
              <Trash2 aria-hidden="true" />
              <span class="visually-hidden">{{ $t('browser.decks.delete', { name: deck.name }) }}</span>
            </button>
          </li>
        </ul>
        <p v-if="removeError" class="notice notice--error" role="alert">{{ tr(removeError) }}</p>
      </template>

      <ConfirmDelete ref="confirm" :title="$t('browser.decks.confirmDelete')" @confirm="deleting && remove(deleting)">
        <template v-if="deleting">
          <HeroPortrait class="confirm-art square" :code="deleting.hero.code" :aspects="deleting.aspects" />
          <span class="confirm-details">
            <strong>{{ deleting.name }}</strong>
            <span class="confirm-meta">{{ deleting.hero.name }}</span>
            <span class="confirm-meta">{{ $t('browser.common.cardCount', deleting.cardCount) }}</span>
          </span>
        </template>
      </ConfirmDelete>
    </main>
  </div>
</template>

<style scoped>
.head { display: flex; align-items: center; gap: var(--space-3); margin-bottom: 16px; }
.head .page-title { margin: 0; }
.new-deck { margin-left: auto; }
.import { display: grid; gap: var(--space-2); margin-bottom: var(--space-5); }
.import .muted { margin: 0; font-size: var(--text-sm); }
.fade-enter-active, .fade-leave-active { transition: opacity 0.2s; }
.fade-enter-from, .fade-leave-to { opacity: 0; }
.toolbar { display: flex; flex-wrap: wrap; align-items: center; gap: var(--space-2); margin-bottom: var(--space-3); padding: var(--space-2); border: 1px solid var(--c-border); border-radius: var(--radius-lg); background: var(--c-surface); }
.search { flex: 1 1 14rem; width: auto; }
.aspects { display: flex; gap: 4px; flex-wrap: wrap; }
.aspect-toggle {
  padding: 5px 10px;
  border: 1px solid color-mix(in srgb, var(--chip) 35%, transparent);
  border-radius: 999px;
  color: var(--c-text-muted);
  font-size: var(--text-xs);
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
}
.aspect-toggle[aria-pressed='true'] { background: color-mix(in srgb, var(--chip) 24%, transparent); border-color: var(--chip); color: var(--c-text); }
.decks { display: grid; grid-template-columns: repeat(auto-fill, minmax(min(100%, 30rem), 1fr)); gap: var(--space-3); }
.deck { display: flex; align-items: stretch; border: 1px solid var(--c-border); border-radius: 14px; background: var(--c-surface); overflow: hidden; transition: border-color var(--transition), transform var(--transition); }
.deck:hover { border-color: var(--c-border-strong); transform: translateY(-1px); }
.deck__link { flex: 1; display: flex; min-width: 0; }
.deck__art { flex: 0 0 8.5rem; min-height: 6.5rem; border-radius: 0; }
.deck__body { flex: 1; min-width: 0; padding: var(--space-3) var(--space-4); display: flex; flex-direction: column; justify-content: center; gap: 2px; }
.deck__name { margin: 0; font-size: 1.35rem; line-height: 1.1; color: var(--title); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.deck__hero { margin: 0; color: var(--c-text-muted); font-size: var(--text-sm); }
.deck__meta { display: flex; flex-wrap: wrap; align-items: center; gap: var(--space-2); margin: var(--space-1) 0 0; color: var(--c-text-faint); font-size: var(--text-xs); }
.chip--warn { color: #f0a4a9; background: rgb(214 80 88 / 0.14); }
.chip--warn svg { width: 12px; height: 12px; }
.deck__remove { display: flex; align-self: center; margin-right: var(--space-3); padding: 8px; border-radius: 8px; color: var(--c-text-faint); }
.deck__remove svg { width: 18px; height: 18px; }
.deck__remove:hover:not(:disabled) { color: var(--c-danger); background: rgb(214 80 88 / 0.1); }
.confirm-art { flex: 0 0 4rem; height: 4rem; border-radius: var(--radius); overflow: hidden; }
.confirm-details { display: flex; flex-direction: column; gap: 2px; min-width: 0; }
.confirm-meta { color: var(--c-text-muted); font-size: var(--text-sm); }
.notice { padding: var(--space-5); border: 1px dashed var(--c-border-strong); border-radius: var(--radius-lg); color: var(--c-text-muted); font-size: var(--text-sm); text-align: center; }
.notice--error { border-style: solid; border-color: var(--c-danger); color: var(--c-danger); }
</style>
