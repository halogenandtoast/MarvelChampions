<script setup lang="ts">
// A question answered by picking a card that is in no play area, as a campaign's Tech
// upgrade or a card searched for is: the cards side by side (all those searched, the ones
// that cannot be picked dimmed) in the middle of the screen, any other answer
// ("no upgrade") as a button beneath. Everyone sees it; the player asked picks.
import { computed, onBeforeUnmount, onMounted } from 'vue'
import { useI18n } from 'vue-i18n'
import { X } from 'lucide-vue-next'
import { useCards } from '../cards'
import { useChoiceLabel, useKeyed } from '../composables/useChoices'
import { skipViewTransitions } from '../composables/useViewTransition'
import { useCardOverlay } from '../composables/useCardOverlay'
import CardFace from './CardFace.vue'
import type { ChoiceView, SearchedView } from '../server'

const { prompt = null, choices, searched = [], canChoose, waitingOn = null, remainingPicks = 1 } = defineProps<{
  prompt?: string | null
  choices: ChoiceView[]
  searched?: SearchedView[]
  canChoose: boolean
  remainingPicks?: number | null
  waitingOn?: string | null
}>()
const emit = defineEmits<{ choose: [index: number]; close: [] }>()
const { t } = useI18n()
const cards = useCards()
const keyed = useKeyed()
const label = useChoiceLabel()
const overlay = useCardOverlay()
const preview = (code: string, element: HTMLElement, touched = false) =>
  overlay.show({ code, name: cards.name(code, code) }, element, cards.landscape(code), touched)
let pressTimer: ReturnType<typeof setTimeout> | undefined
const cancelPress = () => clearTimeout(pressTimer)
const press = (event: PointerEvent, code: string) => {
  cancelPress()
  if (event.pointerType !== 'mouse') {
    const element = event.currentTarget as HTMLElement
    pressTimer = setTimeout(() => preview(code, element, true), 400)
  }
}
const choose = (index: number | null) => {
  if (index === null) return
  overlay.hide()
  const finalPick = remainingPicks === 1
  if (finalPick) skipViewTransitions()
  emit('choose', index)
  if (finalPick) emit('close')
}
const close = () => { skipViewTransitions(); overlay.hide(); emit('close') }
onMounted(skipViewTransitions)
onBeforeUnmount(() => { cancelPress(); overlay.hide() })

const title = computed(() => (prompt ? keyed(prompt) : t('game.prompt.chooseOne')))
type Pick = { key: string | number; code: string; index: number | null }
// a searched card is picked by the choice on it, if any; searched in several places, the
// cards are grouped under where they were
const groups = computed<{ place: string | null; picks: Pick[] }[]>(() =>
  searched.length
    ? searched.map((g) => ({ place: g.place, picks: g.cards.map((s) => ({ key: s.id, code: s.code, index: choices.find((c) => c.target === s.id)?.index ?? null })) }))
    : [{ place: null, picks: choices.filter((c) => c.kind === 'card' && c.code).map((c) => ({ key: c.index, code: c.code!, index: c.index })) }],
)
const picked = computed(() => new Set(groups.value.flatMap((g) => g.picks.map((p) => p.index))))
const others = computed(() => choices.filter((c) => !picked.value.has(c.index)))
</script>

<template>
  <Teleport to="body">
    <div class="pick-backdrop" @click.self="close">
      <article class="pick" role="dialog" aria-modal="true" :aria-label="title">
        <header>
          <h2>{{ title }}</h2>
          <button type="button" class="close" :title="t('common.close')" :aria-label="t('common.close')" @click="close"><X aria-hidden="true" /></button>
        </header>
        <div class="pick-content" @scroll="overlay.hide()">
          <section v-for="g in groups" :key="g.place ?? ''" class="group">
            <h3 v-if="g.place">{{ keyed(g.place) }}</h3>
            <div class="cards">
              <button v-for="c in g.picks" :key="c.key" type="button" class="card" :class="{ unmatched: c.index === null, landscape: cards.landscape(c.code) }" :disabled="!canChoose || c.index === null" @click="choose(c.index)"
                @pointerenter="$event.pointerType === 'mouse' && preview(c.code, $event.currentTarget as HTMLElement)"
                @pointerleave="cancelPress(); overlay.hide()"
                @pointerdown="press($event, c.code)" @pointerup="cancelPress" @pointercancel="cancelPress"
                @focus="preview(c.code, $event.currentTarget as HTMLElement)" @blur="overlay.hide()">
                <span class="face" :class="{ sideways: cards.landscape(c.code) }"><span class="scan"><CardFace :code="c.code" eager /></span></span>
                <span class="name">{{ cards.name(c.code, c.code) }}</span>
              </button>
            </div>
          </section>
        </div>
        <footer>
          <button v-for="c in others" :key="c.index" type="button" class="btn" :disabled="!canChoose" @click="choose(c.index)">{{ label(c) }}</button>
          <p v-if="!canChoose" class="waiting">{{ waitingOn ? t('game.waitingOn', { name: waitingOn }) : t('game.prompt.waiting') }}</p>
        </footer>
      </article>
    </div>
  </Teleport>
</template>

<style scoped>
.pick-backdrop { view-transition-name: card-picker; position: fixed; inset: 0; z-index: 50; display: grid; place-items: center; padding: 16px; background: rgb(0 0 0 / 0.6); }
.pick {
  display: flex;
  flex-direction: column;
  gap: 16px;
  box-sizing: border-box;
  min-width: 0;
  width: min(60rem, 100%);
  max-height: min(80dvh, calc(100dvh - 32px));
  overflow: hidden;
  padding: 20px;
  border: 1px solid var(--line-strong);
  border-radius: 12px;
  background: var(--ink-2);
  color: var(--text);
  box-shadow: 0 8px 24px var(--shadow);
}
.pick-content { min-height: 0; overflow-y: auto; overflow-anchor: none; overscroll-behavior: contain; padding: 4px; }
.pick-content .group + .group { margin-top: 16px; }
header, footer { flex-shrink: 0; }
header { display: flex; align-items: center; gap: 12px; }
h2 { overflow-wrap: anywhere; min-width: 0; margin: 0 auto 0 0; font-size: 1.4rem; text-transform: uppercase; color: var(--title); }
.close { flex-shrink: 0; display: grid; place-items: center; width: 36px; height: 36px; padding: 0; border: 0; border-radius: 8px; background: transparent; color: var(--text-muted); cursor: pointer; }
.close:hover { background: var(--hover-tint); color: var(--text); }
.close svg { width: 20px; height: 20px; }
.group { display: grid; gap: 8px; }
h3 { margin: 0; font-size: 0.75rem; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: var(--text-muted); }
.cards { display: flex; flex-wrap: wrap; justify-content: center; align-items: flex-start; gap: 14px; }
.card { width: 150px; max-width: 100%; display: grid; gap: 8px; padding: 0; border: 0; background: none; color: inherit; font: inherit; cursor: pointer; text-align: center; }
.card:disabled { cursor: default; }
.unmatched .face { filter: grayscale(0.85) brightness(0.6); }
.face { aspect-ratio: 5 / 7; border-radius: 10px; overflow: hidden; border: 2px solid var(--line); transition: border-color 0.12s, transform 0.12s; }
.scan { display: block; width: 100%; height: 100%; }
.card.landscape { width: 210px; }
.face.sideways { aspect-ratio: 7 / 5; }
.card:not(:disabled) .face { border-color: var(--interactive); box-shadow: 0 0 0 2px var(--interactive), 0 0 16px var(--interactive-glow); }
.card:not(:disabled):focus-visible .face { outline: 2px solid var(--interactive); outline-offset: 4px; }
.card:not(:disabled):hover .face { border-color: var(--interactive); transform: translateY(-3px); box-shadow: 0 0 12px var(--interactive-glow); }
.name { font-size: 0.9rem; font-weight: 700; }
footer { display: flex; flex-wrap: wrap; align-items: center; justify-content: flex-end; gap: 10px; }
footer:empty { display: none; }
.waiting { margin: 0 auto 0 0; color: var(--text-muted); font-style: italic; }
</style>

<style>
::view-transition-group(card-picker) { animation: none; }
::view-transition-old(card-picker) { display: none; }
::view-transition-new(card-picker) { animation: none; mix-blend-mode: normal; }
</style>
