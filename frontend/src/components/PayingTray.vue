<script setup lang="ts">
// What is being paid for, held up while it is: the card played, or the card whose
// ability it is, with the resources paid so far and the cards spent on them. A lane of the
// river.
import { computed, ref } from 'vue'
import GameIcon from '../browser/components/GameIcon.vue'
import type { ActiveCostView, ChoiceView } from '../server'
import BoardCard from './BoardCard.vue'

const { cost, things = {}, player = null, elsewhere, declares = [] } = defineProps<{
  cost: ActiveCostView
  things?: Record<string, { code: string; name: string }>
  player?: string | null
  /** ids drawn elsewhere on the board too, which must not share a morph */
  elsewhere: ReadonlySet<string>
  /** the choices declaring a wild resource paid as a type, for the player answering */
  declares?: ChoiceView[]
}>()
const emit = defineEmits<{ choose: [index: number] }>()
// the types a paid resource may be declared as, when it is a wild one: its nth wild
const declaresFor = (i: number) => {
  const n = (cost.wilds ?? []).indexOf(i)
  return n < 0 ? [] : declares.filter((c) => c.target === String(n))
}
const open = ref<number | null>(null)
const declare = (index: number) => {
  open.value = null
  emit('choose', index)
}
const covered = computed(() => !cost.anyAmount && !(cost.owed ?? []).length)
const paying = computed(() => cost.card ?? (cost.source && things[cost.source] ? { id: cost.source, ...things[cost.source] } : null))
</script>

<template>
  <section class="paying" :class="{ covered }" :aria-label="$t('game.paying.title')">
    <header>
      <span class="eyebrow">{{ player ? $t('game.paying.by', { name: player }) : $t('game.paying.title') }}</span>
      <h2>{{ paying?.name ?? $t('game.paying.title') }}</h2>
      <span class="label">{{ $t('game.paying.paid') }}</span>
      <p v-if="cost.paid.length" class="resources">
        <template v-for="(r, i) in cost.paid" :key="i">
          <span v-if="declaresFor(i).length" class="wild">
            <button type="button" class="declarable" :aria-expanded="open === i" :title="$t('game.paying.declareWild')" @click="open = open === i ? null : i">
              <GameIcon :name="r.toLowerCase()" :label="$t(`game.resources.${r.toLowerCase()}`)" />
            </button>
            <span v-if="open === i" class="declare" role="menu" :aria-label="$t('game.paying.declareWild')">
              <button v-for="c in declaresFor(i)" :key="c.index" type="button" role="menuitem" @click="declare(c.index)">
                <GameIcon :name="(c.label ?? '').toLowerCase()" :label="$t(`game.resources.${(c.label ?? '').toLowerCase()}`)" />
              </button>
            </span>
          </span>
          <GameIcon v-else :name="r.toLowerCase()" :label="$t(`game.resources.${r.toLowerCase()}`)" />
        </template>
      </p>
      <p v-else class="none">{{ $t('game.paying.nothingYet') }}</p>
      <p v-if="cost.anyAmount" class="owed">{{ $t('game.paying.anyAmount') }}</p>
      <p v-else-if="covered" class="owed done">{{ $t('game.paying.covered') }}</p>
      <p v-else class="owed">
        {{ $t('game.paying.needs', { count: cost.owed.length }) }}
        <template v-for="(r, i) in cost.owed" :key="i">
          <GameIcon v-if="r" class="due" :name="r.toLowerCase()" :label="$t(`game.resources.${r.toLowerCase()}`)" />
          <span v-else class="due any" :title="$t('game.paying.anyResource')" />
        </template>
      </p>
    </header>
    <BoardCard v-if="paying" :id="paying.id" :code="paying.code" :name="paying.name" :morph="!elsewhere.has(paying.id)" />
    <div v-if="cost.spent.length" class="spent">
      <span class="label">{{ $t('game.paying.spent') }}</span>
      <div class="spent-cards">
        <BoardCard v-for="card in cost.spent" :id="card.id" :key="card.id" :code="card.code" :name="card.name" :morph="!elsewhere.has(card.id)" />
      </div>
    </div>
  </section>
</template>

<style scoped>
.paying {
  --spent-width: calc(var(--card-width) * 0.72);
  display: flex;
  gap: 14px;
  align-items: center;
  flex: 1 1 0;
  min-width: 0;
  padding: 6px 12px;
  border-radius: 14px;
  background: linear-gradient(160deg, rgb(226 163 59 / 0.12), transparent 60%), var(--ink-2);
  border: 1px solid rgb(226 163 59 / 0.4);
  animation: rise 0.25s ease-out;
}
@keyframes rise { from { opacity: 0; transform: translateY(6px); } }
header { display: flex; flex-direction: column; gap: 4px; min-width: 0; flex: 0 1 auto; }
.eyebrow { font-size: 0.62rem; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: var(--amber); }
h2 { margin: 0 0 4px; font-size: 1.2rem; line-height: 1.05; text-transform: uppercase; }
.label { font-size: 0.6rem; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: var(--text-faint); }
.resources { display: flex; flex-wrap: wrap; gap: 4px; margin: 0; font-size: 1.5rem; }
/* a resource as the cards print it: its glyph in a tint of the resource's colour, on that colour */
.resources :deep(.game-icon) { padding: 3px; border-radius: 8px; background: var(--resource, var(--ink-0)); color: var(--resource-tint, rgb(255 255 255 / 0.55)); border: 1px solid rgb(255 255 255 / 0.25); width: 1.25em; height: 1.25em; }
/* a wild paid, which its player may declare as a type: a dashed ring, and the types in a menu */
.wild { position: relative; display: inline-flex; }
.declarable { padding: 0; border: 0; background: none; font: inherit; cursor: pointer; border-radius: 9px; outline: 2px dashed var(--amber); outline-offset: 1px; }
.declare { position: absolute; z-index: 5; top: calc(100% + 6px); left: 50%; transform: translateX(-50%); display: flex; gap: 4px; padding: 5px; border-radius: 10px; background: var(--ink-1, var(--ink-2)); border: 1px solid rgb(226 163 59 / 0.5); box-shadow: 0 6px 18px rgb(0 0 0 / 0.4); }
.declare button { padding: 0; border: 0; background: none; font: inherit; cursor: pointer; }
.declare :deep(.game-icon) { padding: 3px; border-radius: 8px; background: var(--resource, var(--ink-0)); color: var(--resource-tint, rgb(255 255 255 / 0.55)); border: 1px solid rgb(255 255 255 / 0.25); width: 1.1em; height: 1.1em; }
.declare button:hover :deep(.game-icon), .declare button:focus-visible :deep(.game-icon) { border-color: var(--amber); }
.paying.covered { border-color: rgb(79 174 106 / 0.6); background: linear-gradient(160deg, rgb(79 174 106 / 0.14), transparent 60%), var(--ink-2); }
.owed { display: flex; flex-wrap: wrap; align-items: center; gap: 4px; margin: 4px 0 0; font-size: 0.75rem; font-weight: 600; color: var(--amber); }
.owed.done { color: var(--health); }
.owed.done::before { content: '✓'; font-weight: 800; }
.due { width: 1.1rem; height: 1.1rem; opacity: 0.85; }
.due.any { display: inline-block; border-radius: 50%; border: 2px dashed var(--amber); box-sizing: border-box; }
.none { margin: 0; font-size: 0.8rem; font-style: italic; color: var(--text-faint); }
/* spent cards smaller, under their label, to stay within the river's height */
.spent { display: flex; flex-direction: column; gap: 4px; min-width: 0; --card-width: var(--spent-width); }
.spent-cards { display: flex; gap: 6px; overflow-x: auto; }
</style>
