<script setup lang="ts">
// A game before it is dealt: who has sat down, the link to send friends, and a seat to
// take with a deck. The owner may take several, and may start with the seats taken.
import { computed, onMounted, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { Copy, Check } from 'lucide-vue-next'
import DeckPicker from './DeckPicker.vue'
import HeroPortrait from './HeroPortrait.vue'
import { useCards } from '../cards'
import { fetchDecks } from '../browser/lib/api'
import type { DeckList, DeckSummary } from '../browser/types/deck'
import { currentUser } from '../auth'
import { fetchStarterDecks, type DeckChoice } from '../api'
import { heroFace, toDeckChoice, type DeckFace, type DeckKey } from '../deckChoice'
import type { StarterDeck, Table } from '../server'

const { table } = defineProps<{ table: Table }>()
const emit = defineEmits<{ join: [deck: DeckChoice]; leave: []; start: [] }>()
const cards = useCards()

const seats = computed(() => Array.from({ length: table.seats }, (_, position) => table.seated.find((s) => s.position === position) ?? null))
const mine = computed(() => table.seated.filter((s) => s.userId === currentUser.value?.id).length)
const full = computed(() => table.seated.length >= table.seats)
const owner = computed(() => table.owner === currentUser.value?.id)
// heroes are unique: those already brought to a seat
const taken = computed(() => table.seated.flatMap((s) => (s.hero ? [s.hero] : [])))

const link = window.location.href
const copied = ref(false)
const copy = async () => {
  await navigator.clipboard.writeText(link)
  copied.value = true
}

const decks = ref<DeckSummary[]>([])
const starters = ref<StarterDeck[]>([])
// a deck's page sends you to a new game to play it: `?deck=<id>` comes along
const asked = useRoute().query.deck
const deck = ref<DeckKey>(typeof asked === 'string' && /^\d+$/.test(asked) ? `deck:${asked}` : 'starter:spider-man')
const refresh = async () => {
  try {
    decks.value = await fetchDecks()
  } catch {
    // the starter decks are enough to play with
  }
}
onMounted(async () => {
  void refresh()
  starters.value = await fetchStarterDecks().catch(() => [])
})

// start on a starter whose hero nobody has brought
const base = (code: string) => code.replace(/[ab]$/, '')
watch([starters, taken], () => {
  const heroOf = starters.value.find((s) => `starter:${s.key}` === deck.value)?.hero
  if (!heroOf || !taken.value.map(base).includes(base(heroOf))) return
  const free = starters.value.find((s) => !taken.value.map(base).includes(base(s.hero)))
  if (free) deck.value = `starter:${free.key}`
})

// a deck loaded for this game alone, not kept
const loaded = ref<{ face: DeckFace; list: DeckList } | null>(null)
const loading = ref(false)
const choice = (): DeckChoice => (deck.value === 'loaded' && loaded.value ? { list: loaded.value.list } : toDeckChoice(deck.value))
</script>

<template>
  <section class="lobby">
    <header>
      <span class="eyebrow">{{ $t('game.lobby.eyebrow') }}</span>
      <h2>{{ $t('game.lobby.waiting') }}</h2>
    </header>

    <ol class="seats">
      <li v-for="(seat, position) in seats" :key="position" class="seat" :class="{ open: !seat, you: seat?.userId === currentUser?.id }">
        <span class="number">{{ position + 1 }}</span>
        <HeroPortrait v-if="seat?.hero" class="face square" :code="seat.hero" />
        <span v-if="seat" class="who">
          <span class="name">{{ seat.username }}<template v-if="seat.userId === currentUser?.id"> · {{ $t('game.lobby.you') }}</template></span>
          <span v-if="seat.hero" class="hero">{{ cards.name(heroFace(seat.hero), seat.hero) }}</span>
        </span>
        <span v-else class="name">{{ $t('newGame.openSeat') }}</span>
      </li>
    </ol>

    <div class="invite">
      <span>{{ $t('game.lobby.invite') }}</span>
      <div class="link">
        <input class="input" :value="link" type="text" readonly @focus="($event.target as HTMLInputElement).select()" />
        <button type="button" class="btn" @click="copy">
          <Check v-if="copied" aria-hidden="true" /><Copy v-else aria-hidden="true" />
          {{ copied ? $t('game.lobby.copied') : $t('game.lobby.copy') }}
        </button>
      </div>
    </div>

    <template v-if="!full && (owner || mine === 0)">
      <h3>{{ $t('game.lobby.chooseDeck') }}</h3>
      <DeckPicker v-model="deck" v-model:loading="loading" :decks="decks" :starters="starters" :loaded="loaded?.face" :taken="taken" @load="(face, list) => (loaded = { face, list })" @saved="refresh" />
      <button v-if="!loading" type="button" class="btn primary big" @click="emit('join', choice())">{{ $t(mine > 0 ? 'game.lobby.takeAnother' : 'game.lobby.join') }}</button>
    </template>
    <template v-if="mine > 0">
      <p class="hint">{{ $t('game.lobby.dealtWhenFull') }}</p>
      <div class="actions">
        <button v-if="owner" type="button" class="btn primary big" @click="emit('start')">{{ $t('game.lobby.startNow', table.seated.length) }}</button>
        <button type="button" class="btn big" @click="emit('leave')">{{ $t('game.lobby.leave', mine) }}</button>
      </div>
    </template>
  </section>
</template>

<style scoped>
.lobby {
  display: grid;
  gap: 18px;
  width: min(820px, 100%);
  margin: 28px auto;
  padding: 24px;
  border: 1px solid var(--line);
  border-radius: 16px;
  background: linear-gradient(160deg, rgb(184 50 58 / 0.08), transparent 40%), var(--ink-2);
}
header { display: grid; gap: 2px; }
.eyebrow { font-size: 0.65rem; font-weight: 700; letter-spacing: 0.16em; text-transform: uppercase; color: var(--crimson-bright); }
h2, h3 { margin: 0; color: var(--title); text-transform: uppercase; }
h2 { font-size: 2rem; }
h3 { font-size: 1.1rem; letter-spacing: 0.04em; }
.seats { display: grid; grid-template-columns: repeat(auto-fill, minmax(210px, 1fr)); gap: 10px; margin: 0; padding: 0; list-style: none; }
.seat { display: flex; align-items: center; gap: 10px; padding: 12px; border: 1px solid var(--line-strong); border-radius: 10px; background: var(--ink-3); }
.seat.open { border-style: dashed; color: var(--text-muted); background: none; }
.seat.you { border-color: var(--steel); }
.number { display: grid; place-items: center; width: 26px; height: 26px; border-radius: 50%; background: var(--ink-5); font-size: 0.8rem; font-weight: 700; }
.face { width: 34px; height: 34px; border-radius: 8px; overflow: hidden; flex: none; }
.face::after { height: 0; }
.who { display: grid; min-width: 0; }
.hero { font-size: 0.75rem; color: var(--text-muted); }
.name { font-weight: 700; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.invite { display: grid; gap: 6px; font-size: 0.85rem; color: var(--text-muted); }
.link { display: flex; gap: 8px; }
.link .input { flex: 1; min-width: 0; }
.actions { display: flex; gap: 10px; }
.actions .btn { flex: 1; }
.hint { margin: 0; color: var(--text-muted); }
</style>
