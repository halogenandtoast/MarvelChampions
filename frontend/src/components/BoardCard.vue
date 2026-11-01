<script setup lang="ts">
// A card on the table: its scan, the tokens on it, its statuses, and what can be done
// with it. A card the open question is about lights up; a click answers, or asks which
// when it offers several choices, each named on a chip along its foot.
import { computed, ref, useTemplateRef, watch } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import { useChoiceLabel, useChoices } from '../composables/useChoices'
import { useCards, type Back } from '../cards'
import CardFace from './CardFace.vue'
import FoldingIdentity from './FoldingIdentity.vue'
import GameIcon from '../browser/components/GameIcon.vue'
import ChoiceMenu from './ChoiceMenu.vue'
import StatusTokens from './StatusTokens.vue'
import TokenStack from './TokenStack.vue'

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
    exhausted?: boolean
    damage?: number
    threat?: number | null
    /** uses or counters on it */
    counters?: number | null
    /** the cards facedown beneath it, as a Captive ally under Captured by Hydra */
    held?: number | null
    stunCards?: number
    confuseCards?: number
    stunned?: boolean
    confused?: boolean
    tough?: boolean
    toughCards?: number
    bombCounters?: number
    /** what is attached to it or upgrades it, peeking out underneath */
    attached?: Attached[]
    /** a label shown on its top edge, as for a crisis */
    flag?: string | null
    /** a game icon drawn before the flag's label */
    flagIcon?: string | null
    /** an identity's sides, A then B then C: three fold out, as Ant-Man's */
    faces?: string[]
    /** morph between zones as the game changes: off where the card may be drawn twice */
    morph?: boolean
  }>(),
  { morph: true, id: null, code: null, name: '', back: null, damage: 0, threat: null, counters: null, held: null, attached: () => [], flag: null, flagIcon: null },
)

const cards = useCards()
const overlay = useCardOverlay()
const { byTarget, choose, sourceId, assignedDamage } = useChoices()
const label = useChoiceLabel()
const el = useTemplateRef<HTMLElement>('el')

const landscape = computed(() => !props.back && cards.landscape(props.code))
const unfolded = computed(() => props.faces?.length === 3 && props.code === props.faces[2])
const title = computed(() => cards.name(props.code, props.name))
const options = computed(() => (props.id ? (byTarget.value.get(props.id) ?? []) : []))
const displayedDamage = computed(() => props.damage + (props.id ? assignedDamage.value[props.id] ?? 0 : 0))
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

// the card previewed, with its status cards beside it; an attached card shows only itself
const statuses = computed(() => (['stunned', 'confused', 'tough'] as const).filter((s) => props[s]))
const preview = (element: HTMLElement, touched = false, code = props.code, back = props.back) =>
  overlay.show({ code, name: title.value, back, statuses: code === props.code ? statuses.value : [] }, element, !back && cards.landscape(code), touched)

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
  <div class="slot" :class="{ exhausted, landscape, unfolded, hosting: attached.length }">
    <div
      ref="el"
      class="card"
      :class="{ active, exhausted, landscape, unfolded, source }"
      :style="{ viewTransitionName: transitionName }"
      :data-card-id="id ?? undefined"
      :aria-label="title"
      @pointerenter="$event.pointerType === 'mouse' && el && preview(el)"
      @pointerleave="$event.pointerType === 'mouse' && overlay.hide()"
      @pointerdown="press"
      @pointerup="cancelPress"
      @pointercancel="cancelPress"
      @contextmenu="lastPointer !== 'mouse' && $event.preventDefault()"
      @click="click"
    >
      <FoldingIdentity v-if="code && faces?.length === 3" :faces="faces" :code="code" />
      <!-- a two-sided identity turns over as it changes form -->
      <div v-else-if="code && faces?.length === 2" class="flip" :class="{ over: code === faces[1] }">
        <CardFace :code="faces[0]" eager />
        <CardFace :code="faces[1]" eager class="reverse" />
      </div>
      <CardFace v-else :code="code" :back="back" :name="name" />
      <span v-if="flag" class="flag"><GameIcon v-if="flagIcon" :name="flagIcon" :label="flag" /> {{ flag }}</span>
      <StatusTokens class="statuses" :stun-cards="stunCards" :confuse-cards="confuseCards" :stunned="stunned" :confused="confused" :tough="tough" :tough-cards="toughCards" />
      <TokenStack v-if="threat" class="token threat" kind="threat" :count="threat" :aria-label="$t('game.threatOn', { count: threat })" />
      <TokenStack v-if="displayedDamage > 0" class="token damage" kind="damage" :count="displayedDamage" :aria-label="$t('game.damageOn', { count: displayedDamage })" />
      <TokenStack v-if="bombCounters" class="token bombs" kind="generic" :count="bombCounters" :aria-label="$t('game.bombsOn', { count: bombCounters })" />
      <TokenStack v-if="counters" class="token counters" kind="generic" :count="counters" :aria-label="$t('game.countersOn', { count: counters })" />
      <span v-if="held" class="held" :aria-label="$t('game.heldBeneath', { count: held })">{{ $t('game.heldBeneath', { count: held }) }}</span>
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
  transition: margin 0.2s ease, padding 0.25s ease, width 0.4s ease;
}
.slot.unfolded { width: calc(var(--w) / var(--card-aspect)); }
.slot.landscape { width: calc(var(--w) / var(--card-aspect)); }
.slot.unfolded.landscape { width: calc(2 * var(--w)); }
/* The wide spread grows taller when exhausted; keep its meter below the turned card. */
.slot.unfolded.landscape.exhausted { padding-block: max(0px, calc((1.8 * var(--w) - var(--w) / var(--card-aspect)) / 2)); }
/* an exhausted card turns sideways, so it takes its height in width */
.slot.exhausted:not(.landscape) { margin-inline: calc((var(--w) / var(--card-aspect) * 0.9 - var(--w)) / 2); }

.card {
  position: relative;
  width: var(--w);
  aspect-ratio: var(--card-aspect);
  border-radius: calc(var(--w) * 0.06);
  background: var(--ink-3);
  box-shadow: 0 2px 6px rgb(0 0 0 / 0.45), 0 0 0 1px rgb(255 255 255 / 0.1);
  transition: transform 0.25s ease, box-shadow 0.15s ease, width 0.4s ease, aspect-ratio 0.4s ease;
  --book-w: var(--w);
  -webkit-touch-callout: none;
  user-select: none;
  container-type: inline-size;
}
.card.unfolded { width: calc(var(--w) / var(--card-aspect)); aspect-ratio: calc(1 / (2 * var(--card-aspect))); }
.card.landscape { width: calc(var(--w) / var(--card-aspect)); aspect-ratio: calc(1 / var(--card-aspect)); border-radius: calc(var(--w) * 0.05); }
.card.unfolded.landscape { width: calc(2 * var(--w)); aspect-ratio: calc(2 * var(--card-aspect)); }
.card :deep(.card-face) { border-radius: inherit; }
/* nothing behind a card that turns over shows while it is edge on */
.card:has(> .flip, > .folding) { background: none; }
.flip { position: absolute; inset: 0; border-radius: inherit; transform-style: preserve-3d; transform: perspective(1600px) rotateY(0); transition: transform 0.4s ease-in-out; }
.flip.over { transform: perspective(1600px) rotateY(180deg); }
.flip > :deep(.card-face) { position: absolute; inset: 0; backface-visibility: hidden; }
.flip > :deep(.reverse) { transform: rotateY(180deg); }
@media (prefers-reduced-motion: reduce) {
  .flip { transition: none; }
}
.card.exhausted { transform: rotate(90deg) scale(0.9); }
.card.exhausted :deep(.card-face) { filter: brightness(0.82); }
/* lit, not lifted: a lifted card would spill out of its zone's padding */
.card.source { z-index: 3; box-shadow: 0 0 0 2px var(--target), 0 0 22px rgb(255 181 71 / 0.55), 0 12px 24px rgb(0 0 0 / 0.5); }
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

/* tokens: the printed ones, lifted off the art */
.token { position: absolute; --token-size: max(22px, 26cqi); }
.token.damage { top: 4%; right: 4%; }
.token.threat { top: 50%; left: 50%; translate: -50% -50%; --token-size: max(30px, 22cqi); }
.card:not(.landscape) .token.threat { --token-size: max(28px, 34cqi); }
.token.bombs { bottom: 42%; right: 4%; }
.token.counters { bottom: 24%; right: 4%; }
.held {
  position: absolute;
  bottom: 4%;
  left: 4%;
  padding: 0.1rem 0.45rem;
  border-radius: 999px;
  background: var(--shadow);
  color: #fff;
  font-size: 0.6rem;
  font-weight: 700;
}

.card.exhausted .chips { rotate: -90deg; }

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
