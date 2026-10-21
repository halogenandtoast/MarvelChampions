<script setup lang="ts">
// The open question: the choices that are not about a card on the table, as buttons, and
// a hint when others are on the cards themselves. Or who is being waited on, or how the
// game ended.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { Loader, Trophy, Skull } from 'lucide-vue-next'
import { useChoiceLabel, useKeyed } from '../composables/useChoices'
import { useCardOverlay } from '../composables/useCardOverlay'
import { useCards } from '../cards'
import CardFace from './CardFace.vue'
import type { ChoiceView } from '../server'

const { kind = null, prompt: asked = null, buttons = [], onBoard = 0, asking = null, waitingOn = null, outcome = null, things = {} } = defineProps<{
  kind?: string | null
  /** what is asked, as the engine says it */
  prompt?: string | null
  buttons?: ChoiceView[]
  /** how many choices are on cards */
  onBoard?: number
  /** the seat being asked, when it is yours */
  asking?: string | null
  /** the seat being asked, when it is someone else's */
  waitingOn?: string | null
  outcome?: 'Won' | 'Lost' | null
  /** everything in the game by id, to name and show what a choice is about */
  things?: Record<string, { code: string; name: string }>
}>()
const emit = defineEmits<{ choose: [index: number] }>()
const { t, te } = useI18n()
const label = useChoiceLabel()
const cards = useCards()

const keyed = useKeyed()
const prompt = computed(() => (asked ? keyed(asked) : kind && te(`game.prompt.${kind}`) ? t(`game.prompt.${kind}`) : t('game.prompt.chooseOne')))
const overlay = useCardOverlay()
const about = (choice: ChoiceView) => {
  const thing = choice.target ? things[choice.target] : undefined
  const code = choice.code ?? thing?.code ?? null
  return code ? { code, name: thing?.name ?? cards.name(code, '') } : null
}
const text = (choice: ChoiceView) => {
  const thing = choice.target ? things[choice.target] : undefined
  if (choice.kind === 'player' && thing) return thing.name
  return label(choice, about(choice)?.name ?? '', true)
}
const peek = (choice: ChoiceView, event: PointerEvent) => {
  const card = about(choice)
  if (card && event.pointerType === 'mouse') overlay.show(card, event.currentTarget as HTMLElement, cards.landscape(card.code))
}
const tone = (choice: ChoiceView) =>
  choice.kind === 'endTurn' ? 'end' : choice.kind === 'finishPayment' ? 'go' : choice.kind === 'other' ? 'raw' : ''
</script>

<template>
  <section class="question" :class="{ idle: !buttons.length && !onBoard }">
    <template v-if="outcome">
      <div class="outcome" :class="outcome.toLowerCase()">
        <Trophy v-if="outcome === 'Won'" aria-hidden="true" />
        <Skull v-else aria-hidden="true" />
        <span>{{ t(outcome === 'Won' ? 'game.outcome.won' : 'game.outcome.lost') }}</span>
      </div>
    </template>
    <template v-else-if="buttons.length || onBoard">
      <header>
        <span class="eyebrow">{{ asking ? t('game.prompt.asking', { name: asking }) : t('game.prompt.yourMove') }}</span>
        <h2>{{ prompt }}</h2>
      </header>
      <div class="options">
        <button
          v-for="choice in buttons"
          :key="choice.index"
          type="button"
          :class="[tone(choice), { carded: about(choice) && choice.kind !== 'player' }]"
          @click="(overlay.hide(), emit('choose', choice.index))"
          @pointerenter="peek(choice, $event)"
          @pointerleave="overlay.hide()"
        >
          <span v-if="about(choice) && choice.kind !== 'player'" class="thumb"><CardFace :code="about(choice)!.code" /></span>
          <span>{{ text(choice) }}</span>
        </button>
      </div>
      <p v-if="onBoard" class="hint"><span class="swatch" />{{ t('game.prompt.orClickHighlighted', onBoard) }}</p>
    </template>
    <p v-else-if="waitingOn" class="waiting"><Loader class="spin" aria-hidden="true" />{{ t('game.waitingOn', { name: waitingOn }) }}</p>
    <p v-else class="waiting">{{ t('game.prompt.waiting') }}</p>
  </section>
</template>

<style scoped>
.question {
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 14px;
  border-radius: 12px;
  background: linear-gradient(160deg, rgb(76 200 255 / 0.08), transparent 60%), var(--ink-2);
  border: 1px solid rgb(76 200 255 / 0.3);
  max-height: 46dvh;
  overflow-y: auto;
}
.question.idle { background: var(--ink-2); border-color: var(--line); }
header { display: flex; flex-direction: column; gap: 2px; }
.eyebrow { font-size: 0.62rem; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: var(--interactive); }
h2 { margin: 0; font-size: 1.35rem; line-height: 1.05; text-transform: uppercase; color: var(--title); }
.options { display: flex; flex-direction: column; gap: 6px; }
.options button {
  padding: 9px 12px;
  border: 1px solid var(--line-strong);
  border-radius: 8px;
  background: var(--ink-4);
  color: var(--text);
  font: inherit;
  font-size: 0.86rem;
  font-weight: 600;
  text-align: left;
  cursor: pointer;
  transition: background 0.12s, border-color 0.12s;
}
.options button.carded { display: flex; align-items: center; gap: 10px; padding: 5px 10px 5px 5px; }
.thumb { flex: none; width: 30px; height: 30px; border-radius: 6px; overflow: hidden; }
.thumb :deep(.card-face) { object-position: 50% 15%; }
.options button:hover { border-color: var(--interactive); background: color-mix(in srgb, var(--interactive) 16%, var(--ink-4)); }
.options button.end { background: var(--crimson); border-color: var(--crimson); color: #fff; text-align: center; text-transform: uppercase; letter-spacing: 0.06em; font-weight: 700; }
.options button.end:hover { background: var(--crimson-bright); border-color: var(--crimson-bright); }
.options button.go { background: var(--steel-deep); border-color: var(--steel); }
/* the engine's own words for a choice it has no name for yet */
.options button.raw { font-family: ui-monospace, Menlo, monospace; font-size: 0.7rem; font-weight: 400; color: var(--text-muted); overflow-wrap: anywhere; }
.hint { display: flex; align-items: center; gap: 8px; margin: 0; font-size: 0.75rem; color: var(--text-muted); }
.swatch { width: 10px; height: 10px; border-radius: 3px; border: 2px solid var(--interactive); box-shadow: 0 0 6px var(--interactive-glow); }
.waiting { display: flex; align-items: center; gap: 8px; margin: 0; color: var(--text-muted); font-size: 0.85rem; font-style: italic; }
.waiting svg { width: 16px; height: 16px; }
.spin { animation: spin 2s linear infinite; }
@keyframes spin { to { rotate: 360deg; } }
.outcome { display: flex; align-items: center; gap: 10px; font-family: var(--font-display); font-size: 1.6rem; font-weight: 800; text-transform: uppercase; }
.outcome svg { width: 28px; height: 28px; }
.outcome.won { color: var(--health); }
.outcome.lost { color: var(--crimson-bright); }
@media (pointer: coarse) {
  .options button { min-height: 44px; }
}
</style>
