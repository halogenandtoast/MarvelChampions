<script setup lang="ts">
// A question answered by picking a card that is in no play area, as a campaign's Tech
// upgrade is: the cards side by side in the middle of the screen, any other answer
// ("no upgrade") as a button beneath. Everyone sees it; the player asked picks.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { X } from 'lucide-vue-next'
import { useCards } from '../cards'
import { useChoiceLabel, useKeyed } from '../composables/useChoices'
import CardFace from './CardFace.vue'
import type { ChoiceView } from '../server'

const { prompt = null, choices, canChoose, waitingOn = null } = defineProps<{
  prompt?: string | null
  choices: ChoiceView[]
  canChoose: boolean
  waitingOn?: string | null
}>()
const emit = defineEmits<{ choose: [index: number]; close: [] }>()
const { t } = useI18n()
const cards = useCards()
const keyed = useKeyed()
const label = useChoiceLabel()

const title = computed(() => (prompt ? keyed(prompt) : t('game.prompt.chooseOne')))
const picks = computed(() => choices.filter((c) => c.kind === 'card' && c.code))
const others = computed(() => choices.filter((c) => !(c.kind === 'card' && c.code)))
</script>

<template>
  <div class="pick-backdrop" @click.self="emit('close')">
    <article class="pick" role="dialog" aria-modal="true" :aria-label="title">
      <header>
        <h2>{{ title }}</h2>
        <button type="button" class="close" :title="t('common.close')" :aria-label="t('common.close')" @click="emit('close')"><X aria-hidden="true" /></button>
      </header>
      <div class="cards">
        <button v-for="c in picks" :key="c.index" type="button" class="card" :disabled="!canChoose" @click="emit('choose', c.index)">
          <span class="face"><CardFace :code="c.code" eager /></span>
          <span class="name">{{ cards.name(c.code!, c.code!) }}</span>
        </button>
      </div>
      <footer>
        <button v-for="c in others" :key="c.index" type="button" class="btn" :disabled="!canChoose" @click="emit('choose', c.index)">{{ label(c) }}</button>
        <p v-if="!canChoose" class="waiting">{{ waitingOn ? t('game.waitingOn', { name: waitingOn }) : t('game.prompt.waiting') }}</p>
      </footer>
    </article>
  </div>
</template>

<style scoped>
.pick-backdrop { position: fixed; inset: 0; z-index: 50; display: grid; place-items: center; padding: 16px; background: rgb(0 0 0 / 0.6); }
.pick {
  display: grid;
  gap: 16px;
  width: min(60rem, 100%);
  max-height: calc(100dvh - 2rem);
  overflow-y: auto;
  padding: 20px;
  border: 1px solid var(--line-strong);
  border-radius: 12px;
  background: var(--ink-2);
  color: var(--text);
  box-shadow: 0 8px 24px var(--shadow);
}
header { display: flex; align-items: center; gap: 12px; }
h2 { margin: 0 auto 0 0; font-size: 1.4rem; text-transform: uppercase; color: var(--title); }
.close { display: grid; place-items: center; width: 36px; height: 36px; padding: 0; border: 0; border-radius: 8px; background: transparent; color: var(--text-muted); cursor: pointer; }
.close:hover { background: var(--hover-tint); color: var(--text); }
.close svg { width: 20px; height: 20px; }
.cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 14px; }
.card { display: grid; gap: 8px; padding: 0; border: 0; background: none; color: inherit; font: inherit; cursor: pointer; text-align: center; }
.card:disabled { cursor: default; }
.face { aspect-ratio: 5 / 7; border-radius: 10px; overflow: hidden; border: 2px solid var(--line); transition: border-color 0.12s, transform 0.12s; }
.card:not(:disabled):hover .face { border-color: var(--interactive); transform: translateY(-3px); box-shadow: 0 0 12px var(--interactive-glow); }
.name { font-size: 0.9rem; font-weight: 700; }
footer { display: flex; flex-wrap: wrap; align-items: center; justify-content: flex-end; gap: 10px; }
footer:empty { display: none; }
.waiting { margin: 0 auto 0 0; color: var(--text-muted); font-style: italic; }
</style>
