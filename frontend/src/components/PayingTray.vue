<script setup lang="ts">
// What is being paid for, held up while it is: the card played, or the card whose
// ability it is, with the resources paid so far and the cards spent on them. A lane of the
// river.
import { computed, nextTick, ref, useTemplateRef } from 'vue'
import { useEventListener } from '@vueuse/core'
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
const menu = useTemplateRef<HTMLElement>('resourceMenu')
const menuPosition = ref({ top: '0px', left: '0px', '--arrow-left': '16px' })
const menuAbove = ref(false)
const closeMenu = () => {
  menu.value?.hidePopover()
  open.value = null
}
const toggleMenu = async (i: number, event: MouseEvent) => {
  if (open.value === i) {
    closeMenu()
    return
  }
  const anchor = event.currentTarget as HTMLElement
  open.value = i
  await nextTick()
  const panel = menu.value
  if (!panel) return
  panel.showPopover()
  const rect = anchor.getBoundingClientRect()
  const box = panel.getBoundingClientRect()
  const pad = 8
  const gap = 12
  const below = rect.bottom + gap
  menuAbove.value = below + box.height > window.innerHeight - pad
  const top = menuAbove.value ? rect.top - gap - box.height : below
  const left = Math.max(pad, Math.min(rect.left + (rect.width - box.width) / 2, window.innerWidth - box.width - pad))
  menuPosition.value = {
    top: `${Math.max(pad, Math.min(top, window.innerHeight - box.height - pad))}px`,
    left: `${left}px`,
    '--arrow-left': `${Math.max(12, Math.min(rect.left + rect.width / 2 - left, box.width - 12))}px`,
  }
}
useEventListener(window, 'resize', closeMenu)
useEventListener(window, 'scroll', closeMenu, { capture: true })
const declare = (index: number) => {
  closeMenu()
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
          <span v-if="declaresFor(i).length" class="wild-resource">
            <button type="button" class="declarable" :aria-expanded="open === i" :title="$t('game.paying.declareWild')" aria-haspopup="menu" @click="toggleMenu(i, $event)">
              <GameIcon :name="r.toLowerCase()" :label="$t(`game.resources.${r.toLowerCase()}`)" />
            </button>

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
  <Teleport to="body">
    <div ref="resourceMenu" class="declare" :class="{ above: menuAbove }" popover role="menu" :aria-label="$t('game.paying.declareWild')" :style="menuPosition" @toggle="($event as ToggleEvent).newState === 'closed' && (open = null)">
      <button v-for="c in open === null ? [] : declaresFor(open)" :key="c.index" type="button" role="menuitem" @click="declare(c.index)">
        <GameIcon :name="(c.label ?? '').toLowerCase()" :label="$t(`game.resources.${(c.label ?? '').toLowerCase()}`)" />
      </button>
    </div>
  </Teleport>
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
.wild-resource { display: inline-flex; }
.declarable { display: inline-flex; align-items: center; justify-content: center; padding: 0; border: 0; background: none; font: inherit; cursor: pointer; border-radius: 9px; outline: 2px dashed var(--amber); outline-offset: 1px; }
.declare { position: fixed; inset: auto; margin: 0; max-width: calc(100vw - 16px); max-height: calc(100dvh - 16px); overflow: visible; font-size: 1.5rem; gap: 4px; padding: 5px; border-radius: 10px; background: var(--ink-1, var(--ink-2)); border: 1px solid rgb(226 163 59 / 0.5); box-shadow: 0 6px 18px rgb(0 0 0 / 0.4); }
.declare:popover-open { display: flex; }
.declare::before { content: ''; position: absolute; width: 8px; height: 8px; left: var(--arrow-left); top: -5px; transform: translateX(-50%) rotate(45deg); background: var(--ink-1, var(--ink-2)); border-top: 1px solid rgb(226 163 59 / 0.5); border-left: 1px solid rgb(226 163 59 / 0.5); pointer-events: none; }
.declare.above::before { top: auto; bottom: -5px; transform: translateX(-50%) rotate(225deg); }
.declare button { display: inline-flex; flex-shrink: 0; padding: 0; border: 0; background: none; font: inherit; cursor: pointer; }
.declare :deep(.game-icon) { opacity: 0.65; transition: opacity 150ms ease, border-color 150ms ease; padding: 3px; border-radius: 8px; background: var(--resource, var(--ink-0)); color: var(--resource-tint, rgb(255 255 255 / 0.55)); border: 1px solid rgb(255 255 255 / 0.25); width: 1.1em; height: 1.1em; }
.declare button:hover :deep(.game-icon), .declare button:focus-visible :deep(.game-icon) { opacity: 1; border-color: var(--amber); }
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
