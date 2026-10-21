<script setup lang="ts">
// A card on the table: its scan, the tokens on it, its statuses, and what can be done
// with it. A card the open question is about lights up; a click answers, or asks which
// when it offers several choices, each named on a chip along its foot.
import { computed, ref, useTemplateRef, watch } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import { useChoiceLabel, useChoices } from '../composables/useChoices'
import { useCards, type Back } from '../cards'
import CardFace from './CardFace.vue'
import ChoiceMenu from './ChoiceMenu.vue'
import StatusTokens from './StatusTokens.vue'

export interface Attached {
  id: string
  code: string
  name: string
  exhausted?: boolean
  uses?: number
  damage?: number
}

const props = withDefaults(
  defineProps<{
    id?: string | null
    code?: string | null
    name?: string
    back?: Back | null
    size?: 'xs' | 'sm' | 'md' | 'lg' | 'hand'
    exhausted?: boolean
    damage?: number
    hitPoints?: number | null
    threat?: number | null
    /** uses or counters on it */
    counters?: number | null
    stunned?: boolean
    confused?: boolean
    tough?: boolean
    /** what is attached to it or upgrades it, peeking out underneath */
    attached?: Attached[]
    /** a label shown on its top edge, as for a crisis */
    flag?: string | null
    /** morph between zones as the game changes: off where the card may be drawn twice */
    morph?: boolean
  }>(),
  { morph: true, id: null, code: null, name: '', back: null, size: 'md', damage: 0, hitPoints: null, threat: null, counters: null, attached: () => [], flag: null },
)

const cards = useCards()
const overlay = useCardOverlay()
const { byTarget, choose, sourceId } = useChoices()
const label = useChoiceLabel()
const el = useTemplateRef<HTMLElement>('el')

const landscape = computed(() => !props.back && cards.landscape(props.code))
const title = computed(() => cards.name(props.code, props.name))
const options = computed(() => (props.id ? (byTarget.value.get(props.id) ?? []) : []))
const active = computed(() => options.value.length > 0)
// the card the question is about: lifted, and ringed in amber
const source = computed(() => !!props.id && sourceId.value === props.id)
const menuOpen = ref(false)
watch(options, () => (menuOpen.value = false))
// the chips: each distinct thing it can do, named; several of the same are told apart
const chips = computed(() => {
  const seen = new Map<string, number>()
  return options.value.map((option) => {
    const text = label(option, title.value)
    const n = (seen.get(text) ?? 0) + 1
    seen.set(text, n)
    return { option, text: n > 1 ? `${text} ${n}` : text }
  })
})
const left = computed(() => (props.hitPoints === null ? null : Math.max(0, props.hitPoints - props.damage)))

const preview = (element: HTMLElement, touched = false, code = props.code, back = props.back) =>
  overlay.show({ code, name: title.value, back }, element, !back && cards.landscape(code), touched)

let lastPointer = 'mouse'
let pressTimer: ReturnType<typeof setTimeout> | undefined
const cancelPress = () => clearTimeout(pressTimer)
const press = (event: PointerEvent) => {
  lastPointer = event.pointerType
  cancelPress()
  if (event.pointerType !== 'mouse' && el.value) pressTimer = setTimeout(() => el.value && preview(el.value, true), 400)
}

const click = () => {
  if (options.value.length > 1) {
    overlay.hide()
    menuOpen.value = !menuOpen.value
  } else if (options.value[0]) {
    choose(options.value[0].index)
  } else if (lastPointer !== 'mouse' && el.value) {
    preview(el.value, true)
  }
}

const attachedChoice = (a: Attached) => byTarget.value.get(a.id)?.[0] ?? null
const transitionName = computed(() => (props.morph && props.id ? `c${props.id.replace(/[^a-zA-Z0-9-]/g, '')}` : undefined))
</script>

<template>
  <div class="slot" :class="[`size-${size}`, { exhausted, landscape, hosting: attached.length }]">
    <div
      ref="el"
      class="card"
      :class="{ active, exhausted, landscape, source }"
      :style="{ viewTransitionName: transitionName }"
      :data-card-id="id ?? undefined"
      :title="title"
      @pointerenter="$event.pointerType === 'mouse' && el && preview(el)"
      @pointerleave="$event.pointerType === 'mouse' && overlay.hide()"
      @pointerdown="press"
      @pointerup="cancelPress"
      @pointercancel="cancelPress"
      @contextmenu="lastPointer !== 'mouse' && $event.preventDefault()"
      @click="click"
    >
      <CardFace :code="code" :back="back" :name="name" />
      <span v-if="flag" class="flag">{{ flag }}</span>
      <StatusTokens class="statuses" :stunned="stunned" :confused="confused" :tough="tough" />
      <span v-if="threat !== null" class="token threat" :title="$t('game.threatOn', { count: threat })">{{ threat }}</span>
      <span v-if="damage > 0" class="token damage" :title="$t('game.damageOn', { count: damage })">{{ damage }}</span>
      <span v-if="counters" class="token counters" :title="$t('game.countersOn', { count: counters })">{{ counters }}</span>
      <span v-if="left !== null" class="hp" :class="{ hurt: damage > 0 }" :title="$t('game.hitPointsLeft', { left, total: hitPoints })">
        <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M12 21s-7.5-4.6-9.6-9.3C1 8.4 3 5 6.4 5c2 0 3.4 1.1 4.1 2.3h3C14.2 6.1 15.6 5 17.6 5 21 5 23 8.4 21.6 11.7 19.5 16.4 12 21 12 21Z" fill="currentColor"/></svg>
        {{ left }}<small>/{{ hitPoints }}</small>
      </span>
      <div v-if="chips.length" class="chips">
        <button v-for="chip in chips" :key="chip.option.index" type="button" class="chip" @click.stop="choose(chip.option.index)">{{ chip.text }}</button>
      </div>
    </div>
    <ul v-if="attached.length" class="attached">
      <li
        v-for="a in attached"
        :key="a.id"
        :class="{ active: attachedChoice(a), exhausted: a.exhausted }"
        :data-card-id="a.id"
        @pointerenter="$event.pointerType === 'mouse' && preview($event.currentTarget as HTMLElement, false, a.code, null)"
        @pointerleave="$event.pointerType === 'mouse' && overlay.hide()"
        @click.stop="attachedChoice(a) ? choose(attachedChoice(a)!.index) : preview($event.currentTarget as HTMLElement, true, a.code, null)"
      >
        <span class="attached-name">{{ cards.name(a.code, a.name) }}</span>
        <span v-if="a.uses" class="attached-uses">{{ a.uses }}</span>
        <span v-if="a.damage" class="attached-damage">{{ a.damage }}</span>
      </li>
    </ul>
    <ChoiceMenu
      v-if="menuOpen && el"
      :options="options"
      :anchor="el"
      :card-name="title"
      @choose="(index) => ((menuOpen = false), choose(index))"
      @close="menuOpen = false"
    />
  </div>
</template>

<style scoped>
.slot {
  --w: var(--card-width);
  position: relative;
  flex: none;
  display: flex;
  flex-direction: column;
  align-items: center;
  width: var(--w);
  transition: margin 0.2s ease;
}
.size-xs { --w: calc(var(--card-width) * 0.62); }
.size-sm { --w: calc(var(--card-width) * 0.82); }
.size-lg { --w: calc(var(--card-width) * 1.32); }
.size-hand { --w: calc(var(--card-width) * 1.08); }
.slot.landscape { width: calc(var(--w) / var(--card-aspect)); }
/* an exhausted card turns sideways, so it takes its height in width */
.slot.exhausted:not(.landscape) { margin-inline: calc((var(--w) / var(--card-aspect) * 0.9 - var(--w)) / 2); }

.card {
  position: relative;
  width: var(--w);
  aspect-ratio: var(--card-aspect);
  border-radius: calc(var(--w) * 0.06);
  background: var(--ink-3);
  box-shadow: 0 2px 6px rgb(0 0 0 / 0.45), 0 0 0 1px rgb(255 255 255 / 0.1);
  transition: transform 0.25s ease, box-shadow 0.15s ease;
  -webkit-touch-callout: none;
  user-select: none;
  container-type: inline-size;
}
.card.landscape { width: calc(var(--w) / var(--card-aspect)); aspect-ratio: calc(1 / var(--card-aspect)); border-radius: calc(var(--w) * 0.05); }
.card :deep(.card-face) { border-radius: inherit; }
.card.exhausted { transform: rotate(90deg) scale(0.9); }
.card.exhausted :deep(.card-face) { filter: brightness(0.82); }
.card.source { z-index: 3; transform: translateY(-6px) scale(1.06); box-shadow: 0 0 0 2px var(--target), 0 0 22px rgb(255 181 71 / 0.55), 0 12px 24px rgb(0 0 0 / 0.5); }
.card.source.exhausted { transform: rotate(90deg) scale(0.96); }
.card.source.active { box-shadow: 0 0 0 2px var(--target), 0 0 0 5px var(--interactive), 0 0 22px var(--interactive-glow); }
.card.active { cursor: pointer; box-shadow: 0 0 0 2px var(--interactive), 0 0 16px var(--interactive-glow); }
.card.active:hover { box-shadow: 0 0 0 3px var(--interactive), 0 0 24px var(--interactive-glow); }

.flag {
  position: absolute;
  top: 6%;
  left: 50%;
  translate: -50% 0;
  padding: 1px 8px;
  border-radius: 999px;
  background: var(--crimson);
  color: #fff;
  font-size: 0.6rem;
  font-weight: 800;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  box-shadow: 0 2px 6px var(--shadow);
}
.statuses { position: absolute; top: 4%; left: 4%; }

/* tokens: round, a heavy number, lifted off the art */
.token {
  position: absolute;
  display: grid;
  place-items: center;
  min-width: max(22px, 26cqi);
  height: max(22px, 26cqi);
  padding: 0 4px;
  border-radius: 999px;
  color: #fff;
  font-family: var(--font-display);
  font-size: max(14px, 17cqi);
  font-weight: 800;
  line-height: 1;
  border: 2px solid rgb(255 255 255 / 0.85);
  box-shadow: 0 2px 6px rgb(0 0 0 / 0.6);
  text-shadow: 0 1px 2px rgb(0 0 0 / 0.5);
}
.token.damage { top: 4%; right: 4%; background: radial-gradient(circle at 35% 30%, #e05a61, var(--crimson-deep)); }
.token.threat { top: 50%; left: 50%; translate: -50% -50%; min-width: max(30px, 22cqi); height: max(30px, 22cqi); font-size: max(18px, 15cqi); background: radial-gradient(circle at 35% 30%, #f2c063, var(--amber-deep)); }
.card:not(.landscape) .token.threat { min-width: max(28px, 34cqi); height: max(28px, 34cqi); font-size: max(17px, 22cqi); }
.token.counters { bottom: 24%; right: 4%; background: radial-gradient(circle at 35% 30%, #8fb6da, var(--steel-deep)); }

/* hit points left, along the bottom */
.hp {
  position: absolute;
  left: 50%;
  bottom: 4%;
  translate: -50% 0;
  display: inline-flex;
  align-items: center;
  gap: 3px;
  padding: 2px 7px 2px 5px;
  border-radius: 999px;
  background: rgb(10 12 16 / 0.86);
  border: 1px solid rgb(79 174 106 / 0.6);
  color: #d9f2df;
  font-family: var(--font-display);
  font-size: max(12px, 13cqi);
  font-weight: 700;
  line-height: 1;
  white-space: nowrap;
}
.hp svg { width: 1em; height: 1em; color: var(--health); }
.hp small { font-size: 0.75em; color: var(--text-faint); font-weight: 600; }
.hp.hurt { border-color: rgb(214 80 88 / 0.7); }
.hp.hurt svg { color: var(--danger); }
.card.exhausted .hp, .card.exhausted .chips { rotate: -90deg; }

/* what it can do, as chips over its foot */
.chips {
  position: absolute;
  inset: auto 3% 16% 3%;
  display: flex;
  flex-direction: column;
  align-items: stretch;
  gap: 3px;
  z-index: 2;
}
.card.landscape .chips { bottom: 22%; inset-inline: 18%; }
.chip {
  padding: 3px 4px;
  border: 1px solid var(--interactive);
  border-radius: 6px;
  background: rgb(8 24 34 / 0.9);
  color: #dff5ff;
  font: inherit;
  font-size: max(10px, 9.5cqi);
  font-weight: 700;
  line-height: 1.15;
  text-align: center;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  cursor: pointer;
  box-shadow: 0 2px 6px rgb(0 0 0 / 0.5);
}
.chip:hover { background: var(--interactive); color: #04121a; }

/* attachments and upgrades: a strip each, tucked under the card */
.attached {
  display: flex;
  flex-direction: column;
  gap: 2px;
  width: 100%;
  margin: 4px 0 0;
  padding: 0;
  list-style: none;
}
.slot.landscape .attached { width: 100%; }
.attached li {
  display: flex;
  align-items: center;
  gap: 4px;
  padding: 3px 6px;
  border-radius: 5px;
  background: var(--ink-4);
  border: 1px solid var(--line-strong);
  border-left: 3px solid var(--steel);
  font-size: 0.66rem;
  font-weight: 600;
  line-height: 1.2;
  cursor: help;
}
.attached li.exhausted { opacity: 0.6; font-style: italic; }
.attached li.active { cursor: pointer; border-color: var(--interactive); box-shadow: 0 0 8px var(--interactive-glow); }
.attached-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.attached-uses, .attached-damage { flex: none; padding: 0 5px; border-radius: 999px; font-weight: 800; }
.attached-uses { background: var(--steel-deep); }
.attached-damage { background: var(--crimson-deep); }
</style>
