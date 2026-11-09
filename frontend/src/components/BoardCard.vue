<script setup lang="ts">
import { useCardDebug } from '../composables/useCardDebug'
import ActionLabel from './ActionLabel.vue'
// A card on the table: its scan, the tokens on it, its statuses, and what can be done
// with it. A card the open question is about lights up; a click answers, or asks which
// when it offers several choices, each named on a chip along its foot.
import { computed, onBeforeUnmount, onMounted, ref, useTemplateRef, watch } from 'vue'
import { useMediaQuery } from '@vueuse/core'
import { useCardOverlay } from '../composables/useCardOverlay'
import { useChoiceLabel, useChoiceName, useChoices } from '../composables/useChoices'
import { useCards, type Back } from '../cards'
import { parseInput } from '../i18n'
import CardFace from './CardFace.vue'
import FoldingIdentity from './FoldingIdentity.vue'
import GameIcon from '../browser/components/GameIcon.vue'
import ChoiceMenu from './ChoiceMenu.vue'
import StatusTokens from './StatusTokens.vue'
import { statusCards, inactiveStatusCards } from '../statusCards'
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
    /** Printed metagame spaces, in reading order; occupied indices are saved by the engine. */
    resourceSlots?: string[]
    occupiedSlots?: number[]
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
    /** Public boost information from a facedown card beneath this card. */
    tuckedBoostIcons?: number | null
    tuckedBoostStar?: boolean | null
    /** an identity's sides, A then B then C: three fold out, as Ant-Man's */
    faces?: string[]
    /** morph between zones as the game changes: off where the card may be drawn twice */
    morph?: boolean
    /** turn from a facedown card to its face without moving its slot */
    flipReveal?: boolean
  }>(),
  { morph: true, id: null, code: null, name: '', back: null, damage: 0, threat: null, counters: null, held: null, attached: () => [], flag: null, flagIcon: null },
)

const mobile = useMediaQuery('(max-width: 760px)')
const cards = useCards()
const revealBack = ref<Back | null>(props.back)
watch(() => props.back, back => { if (back) revealBack.value = back })
const overlay = useCardOverlay()
const { byTarget, choose, sourceId, assignedDamage, hoveredTarget } = useChoices()
const clearHover = () => {
  if (hoveredTarget.value === props.id) hoveredTarget.value = null
}
onBeforeUnmount(clearHover)
const label = useChoiceLabel()
const actionName = useChoiceName()
const el = useTemplateRef<HTMLElement>('el')
const threatPulse = ref(0)
const damagePulse = ref(0)
let threatPulseTimer: ReturnType<typeof setTimeout> | undefined
let damagePulseTimer: ReturnType<typeof setTimeout> | undefined
watch(() => props.threat, (amount, previous) => {
  if ((amount ?? 0) <= (previous ?? 0)) return
  clearTimeout(threatPulseTimer)
  threatPulse.value++
  threatPulseTimer = setTimeout(() => { threatPulse.value = 0 }, 850)
})
watch(() => props.damage, (amount, previous) => {
  if (amount <= previous) return
  clearTimeout(damagePulseTimer)
  damagePulse.value++
  damagePulseTimer = setTimeout(() => { damagePulse.value = 0 }, 850)
})
onBeforeUnmount(() => { clearTimeout(threatPulseTimer); clearTimeout(damagePulseTimer) })
const cancelThreatPulse = () => { clearTimeout(threatPulseTimer); clearTimeout(damagePulseTimer); threatPulse.value = 0; damagePulse.value = 0 }
onMounted(() => window.addEventListener('marvel:cancel-animations', cancelThreatPulse))
onBeforeUnmount(() => window.removeEventListener('marvel:cancel-animations', cancelThreatPulse))

const landscape = computed(() => !props.back && cards.landscape(props.code))
const unfolded = computed(() => props.faces?.length === 3 && props.code === props.faces[2])
const title = computed(() => cards.name(props.code, props.name))
const revealStarted = computed(() => props.flipReveal && revealBack.value && !props.back)
const options = computed(() => (props.id ? (byTarget.value.get(props.id) ?? []) : [])
  .filter(option => !(revealStarted.value && option.label === '$label.revealBoostCards')))
const displayedDamage = computed(() => props.damage + (props.id ? assignedDamage.value[props.id] ?? 0 : 0))
const active = computed(() => options.value.length > 0)
// the card the question is about: lifted, and ringed in amber
const source = computed(() => !!props.id && sourceId.value === props.id && !(revealStarted.value && byTarget.value.get(props.id)?.some(option => option.label === '$label.revealBoostCards')))
const menuOpen = ref(false)
watch(options, () => (menuOpen.value = false))
const slotChoices = computed(() => {
  const found = new Map<number, (typeof options.value)[number]>()
  if (!props.resourceSlots?.length) return found
  for (const option of options.value) {
    if (!option.label?.startsWith('$label.metagameSlot ')) continue
    const { params } = parseInput(option.label)
    const slot = Number(params.count) - 1
    if (Number.isInteger(slot) && slot >= 0 && slot < props.resourceSlots.length) found.set(slot, option)
  }
  return found
})
// the chips: each distinct thing it can do, named; several of the same are told apart
const chips = computed(() => {
  const seen = new Map<string, number>()
  return options.value.filter((option) => ![...slotChoices.value.values()].some((slot) => slot.index === option.index)).map((option) => {
    const basic = option.abilityType === 'Basic' || /^\$label\.(changeForm|changeTo[AB]|attack|thwart|recover|defend)\b/.test(option.label ?? '')
    const text = label(option.kind === 'ability' && !basic ? { ...option, label: null } : option, title.value)
    const n = (seen.get(text) ?? 0) + 1
    seen.set(text, n)
    return { option, text: n > 1 ? `${text} ${n}` : text }
  })
})
// Replace crowded action stacks with one menu containing every action.
// Several effects of the same interrupt/response are one entry point; the
// player picks the effect from the menu after opening that ability.
const groupedReaction = computed(() => {
  const first = options.value[0]
  return options.value.length > 1 && first?.kind === 'ability' && /Response|Interrupt/.test(first.abilityType ?? '')
    && options.value.every((option) => option.kind === 'ability' && option.abilityType === first.abilityType)
      ? label({ ...first, label: null }, title.value)
      : null
})
const visibleChips = computed(() => groupedReaction.value || chips.value.length > 3 ? [] : chips.value)
const overflowChips = computed(() => groupedReaction.value || chips.value.length > 3 ? chips.value : [])
const overflowOpen = ref(false)
watch(options, () => (overflowOpen.value = false))
const toggleOverflow = () => {
  overlay.hide()
  menuOpen.value = false
  overflowOpen.value = !overflowOpen.value
}

// the card previewed, with its status cards beside it; an attached card shows only itself
const statuses = computed(() => statusCards(props))
const preview = (element: HTMLElement, touched = false, code = props.code, back = props.back) =>
  overlay.show({ code, name: title.value, back, statuses: code === props.code ? statuses.value : [], inactiveStatuses: code === props.code ? inactiveStatusCards(props) : [] }, element, !back && cards.landscape(code), touched)

const cardDebug = useCardDebug()
let debugHeld = false
let pressOrigin = { x: 0, y: 0 }
const openDebug = () => { if (cardDebug?.enabled.value && props.id) { overlay.hide(); cardDebug.open(props.id, { code: props.code ?? '', name: title.value }); return true } return false }
let lastPointer = 'mouse'
let pressTimer: ReturnType<typeof setTimeout> | undefined
const cancelPress = () => clearTimeout(pressTimer)
onBeforeUnmount(cancelPress)
const press = (event: PointerEvent) => {
  debugHeld = false
  pressOrigin = { x: event.clientX, y: event.clientY }
  lastPointer = event.pointerType
  cancelPress()
  if (cardDebug?.enabled.value && props.id) pressTimer = setTimeout(() => { debugHeld = true; openDebug() }, 500)
  else if (event.pointerType !== 'mouse' && el.value) pressTimer = setTimeout(() => el.value && preview(el.value, true), 400)
}

const click = () => {
  if (debugHeld) { debugHeld = false; return }
  overflowOpen.value = false
  if (mobile.value && el.value) {
    preview(el.value, true)
  } else if (options.value.length > 1) {
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
      :class="{ active, exhausted, landscape, unfolded, source, 'generic-actions': chips.length && (counters || bombCounters) }"
      :style="{ viewTransitionName: transitionName }"
      :data-card-id="id ?? undefined"
      :aria-label="mobile && code ? $t('game.mobile.inspectCard', { name: title }) : title"
      :role="(mobile || cardDebug?.enabled.value) && code ? 'button' : undefined"
      :tabindex="(mobile || cardDebug?.enabled.value) && code ? 0 : undefined"
      @keydown="cardDebug?.enabled.value && $event.shiftKey && $event.key === 'F10' ? ($event.preventDefault(), openDebug()) : mobile && ['Enter', ' '].includes($event.key) && ($event.preventDefault(), click())"
      @pointerenter="$event.pointerType === 'mouse' && el && (hoveredTarget = id, preview(el))"
      @pointerleave="cancelPress(); $event.pointerType === 'mouse' && (clearHover(), overlay.hide())"
      @focusin="hoveredTarget = id"
      @focusout="clearHover"
      @pointerdown="press"
      @pointerup="cancelPress"
      @pointercancel="cancelPress"
      @pointermove="Math.hypot($event.clientX - pressOrigin.x, $event.clientY - pressOrigin.y) > 10 && cancelPress()"
      @contextmenu="(cardDebug?.enabled.value ? (cancelPress(), openDebug()) : lastPointer !== 'mouse') && $event.preventDefault()"
      @click.capture="debugHeld && ($event.preventDefault(), $event.stopImmediatePropagation(), debugHeld = false)"
      @click="click"
    >
      <div v-if="flipReveal && revealBack" class="reveal-flip" :class="{ revealed: !back }">
        <CardFace :code="code" :name="name" eager class="reveal-front" :aria-hidden="!!back" />
        <CardFace :back="revealBack" eager class="reveal-back" :aria-hidden="!back" />
      </div>
      <FoldingIdentity v-else-if="code && faces?.length === 3" :faces="faces" :code="code" />
      <!-- a two-sided identity turns over as it changes form -->
      <div v-else-if="code && faces?.length === 2" class="flip" :class="{ over: code === faces[1] }">
        <CardFace :code="faces[0]" eager />
        <CardFace :code="faces[1]" eager class="reverse" />
      </div>
      <CardFace v-else :code="code" :back="back" :name="name" />
      <span v-if="flag" class="flag"><GameIcon v-if="flagIcon" :name="flagIcon" :label="flag" /> {{ flag }}</span>
      <span v-if="tuckedBoostIcons != null" class="boost-badge" role="img" :aria-label="$t(tuckedBoostStar ? 'game.tuckedBoostIconsWithStar' : 'game.tuckedBoostIcons', { count: tuckedBoostIcons })">
        <span aria-hidden="true" class="boost-badge-content"><GameIcon name="boost" /> <span>{{ tuckedBoostIcons }}</span><GameIcon v-if="tuckedBoostStar" name="star" /></span>
      </span>
      <StatusTokens class="statuses" :stun-cards="stunCards" :confuse-cards="confuseCards" :stunned="stunned" :confused="confused" :tough="tough" :tough-cards="toughCards" />
      <div v-if="resourceSlots?.length" class="metagame-board" :class="{ 'three-columns': resourceSlots.length === 9 }" :aria-label="$t('game.metagameBoard')">
        <button v-for="(resource, index) in resourceSlots" :key="index" type="button" class="metagame-space" :class="{ occupied: occupiedSlots?.includes(index) }" :disabled="!slotChoices.has(index)" :aria-label="$t(occupiedSlots?.includes(index) ? 'game.occupiedResourceSpace' : 'game.emptyResourceSpace', { resource, space: index + 1 })" @click.stop="slotChoices.get(index) && choose(slotChoices.get(index)!.index)">
          <span class="space-number" aria-hidden="true">{{ index + 1 }}</span>
          <GameIcon :name="resource.toLowerCase()" />
          <span v-if="occupiedSlots?.includes(index)" class="space-token" aria-hidden="true">●</span>
        </button>
      </div>
      <TokenStack v-if="threat" class="token threat" kind="threat" :count="threat" :pulse="threatPulse" :aria-label="$t('game.threatOn', { count: threat })" />
      <TokenStack v-if="displayedDamage > 0" class="token damage" kind="damage" :count="displayedDamage" :pulse="damagePulse" :aria-label="$t('game.damageOn', { count: displayedDamage })" />
      <TokenStack v-if="bombCounters" class="token bombs" kind="generic" :count="bombCounters" :aria-label="$t('game.bombsOn', { count: bombCounters })" />
      <TokenStack v-if="counters" class="token counters" kind="generic" :count="counters" :aria-label="$t('game.countersOn', { count: counters })" />
      <span v-if="held" class="held" :aria-label="$t('game.heldBeneath', { count: held })">{{ $t('game.heldBeneath', { count: held }) }}</span>
      <div v-if="chips.length" class="chips">
        <button v-for="chip in visibleChips" :key="chip.option.index" type="button" class="chip" @click.stop="choose(chip.option.index)"><ActionLabel :text="chip.text" :name="actionName(chip.option, code)" /></button>
        <button v-if="overflowChips.length" type="button" class="chip" :aria-label="groupedReaction ?? $t('label.moreActions')" aria-haspopup="menu" :aria-expanded="overflowOpen" @click.stop="toggleOverflow">{{ groupedReaction ?? '…' }}</button>
      </div>
    </div>
    <ul v-if="attached.length" class="attached">
      <li
        v-for="a in attached"
        :key="a.id"
        :class="{ active: attachedChoice(a), emphasized: attachedChoice(a) && hoveredTarget === a.id, exhausted: a.exhausted }"
        :data-card-id="a.id"
        @pointerenter="$event.pointerType === 'mouse' && preview($event.currentTarget as HTMLElement, false, a.code, null)"
        @pointerleave="cancelPress(); $event.pointerType === 'mouse' && overlay.hide()"
        @click.stop="attachedChoice(a) ? choose(attachedChoice(a)!.index) : preview($event.currentTarget as HTMLElement, true, a.code, null)"
      >
        <span class="attached-name">{{ cards.name(a.code, a.name) }}</span>
        <span v-if="a.uses" class="attached-uses">{{ a.uses }}</span>
        <span v-if="a.damage" class="attached-damage">{{ a.damage }}</span>
      </li>
    </ul>
    <ChoiceMenu
      v-if="overflowOpen && el"
      :options="overflowChips.map(chip => chip.option)"
      :anchor="el"
      :card-name="title"
      @choose="(index) => ((overflowOpen = false), choose(index))"
      @close="overflowOpen = false"
    />
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
/* Unfold the artwork while keeping controls at the normal card scale. */
.card.unfolded { --control-unit: calc(var(--w) / 100); width: calc(var(--w) / var(--card-aspect)); aspect-ratio: calc(1 / (2 * var(--card-aspect))); }
.card.landscape { width: calc(var(--w) / var(--card-aspect)); aspect-ratio: calc(1 / var(--card-aspect)); border-radius: calc(var(--w) * 0.05); }
.card.unfolded.landscape { width: calc(2 * var(--w)); aspect-ratio: calc(2 * var(--card-aspect)); }
.card :deep(.card-face) { border-radius: inherit; }
.card:has(> .reveal-flip) { perspective: 900px; }
.reveal-flip { position: absolute; inset: 0; border-radius: inherit; transform-style: preserve-3d; transition: transform 560ms cubic-bezier(0.4, 0, 0.2, 1); }
.reveal-flip.revealed { transform: rotateY(180deg); }
.reveal-flip > :deep(.card-face) { position: absolute; inset: 0; backface-visibility: hidden; -webkit-backface-visibility: hidden; }
.reveal-flip > :deep(.reveal-front) { transform: rotateY(180deg); }
@media (prefers-reduced-motion: reduce) { .reveal-flip { transition: none; } }
/* Landscape scans vary slightly in proportion: let the image set the frame's
   height instead of cropping it or leaving space inside a fixed frame. */
.card.landscape:not(.unfolded):has(> img.card-face) { aspect-ratio: auto; }
.card.landscape:not(.unfolded) > :deep(img.card-face) { height: auto; }
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
.boost-badge {
  position: absolute;
  top: 8%;
  left: 50%;
  translate: -50% 0;
  padding: 0.2em 0.45em;
  border: 1px solid var(--line-strong);
  border-radius: 0.35em;
  background: var(--ink-2);
  color: var(--text);
  box-shadow: 0 2px 5px var(--shadow);
  font-size: max(12px, calc(var(--control-unit, 1cqi) * 12));
  font-weight: 800;
  font-variant-numeric: tabular-nums;
  line-height: 1;
  pointer-events: none;
}
.boost-badge-content { display: flex; align-items: center; gap: 0.25em; white-space: nowrap; }
.statuses { position: absolute; top: 4%; left: 4%; }
.metagame-board { position: absolute; top: 9%; left: 12%; right: 12%; display: grid; grid-template-columns: repeat(2, 1fr); gap: 3px; padding: 4px; border-radius: 5px; background: var(--ink-2); border: 1px solid var(--line-strong); }
.metagame-board.three-columns { grid-template-columns: repeat(3, 1fr); }
.metagame-space { position: relative; display: grid; place-items: center; min-height: 26px; padding: 2px; background: transparent; color: inherit; border: 1px solid var(--line-strong); border-radius: 3px; font-size: max(12px, calc(var(--control-unit, 1cqi) * 12)); }
.metagame-space:not(:disabled) { border-color: var(--interactive); cursor: pointer; }
.metagame-space:not(:disabled):hover, .metagame-space:focus-visible { outline: 2px solid var(--interactive); outline-offset: 1px; background: var(--ink-4); }
.metagame-space.occupied { background: var(--ink-4); }
.space-number { position: absolute; top: 1px; left: 2px; color: var(--text-muted); font: 9px/1 sans-serif; }
.space-token { position: absolute; inset: 0; display: grid; place-items: center; color: var(--amber); text-shadow: 0 1px 2px var(--ink-1); font-size: 1.35em; }

/* tokens: the printed ones, lifted off the art */
.token { position: absolute; --token-size: max(22px, calc(var(--control-unit, 1cqi) * 26)); }
.token.damage { top: 4%; right: 4%; }
.token.threat { top: 50%; left: 50%; translate: -50% -50%; --token-size: max(30px, calc(var(--control-unit, 1cqi) * 22)); }
.card:not(.landscape) .token.threat, .card.unfolded .token.threat { --token-size: max(28px, calc(var(--control-unit, 1cqi) * 34)); }
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
  inset: auto 3% 4% 3%;
  display: flex;
  flex-direction: column;
  align-items: stretch;
  gap: 3px;
  z-index: 2;
}
.card.landscape .chips { bottom: 22%; inset-inline: 18%; }
/* Keep counters above the action area without changing the table's card spacing. */
.card.generic-actions .token.counters { top: 20%; bottom: auto; }
.card.generic-actions .token.bombs { top: 39%; bottom: auto; }
.card.generic-actions .chips { inset: auto 3% 4%; max-height: 56%; overflow-y: auto; }
.card.generic-actions .chip { flex-shrink: 0; }
.card.unfolded .chips { bottom: 4%; inset-inline: auto; left: 50%; translate: -50% 0; width: calc(var(--w) * 0.94); }
.card.unfolded.exhausted .chips { top: 50%; bottom: auto; translate: -50% -50%; }
.chip {
  padding: 3px 4px;
  border: 1px solid var(--interactive);
  border-radius: 6px;
  background: rgb(8 24 34 / 0.9);
  color: #dff5ff;
  font: inherit;
  font-size: max(10px, calc(var(--control-unit, 1cqi) * 9.5));
  font-weight: 700;
  line-height: 1.15;
  text-align: center;
  white-space: normal;
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
.attached li.active.emphasized { background: color-mix(in srgb, var(--interactive) 25%, var(--ink-2)); color: var(--title); outline: 2px solid var(--interactive); outline-offset: 2px; box-shadow: 0 0 20px var(--interactive-glow); }
.attached-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.attached-uses, .attached-damage { flex: none; padding: 0 5px; border-radius: 999px; font-weight: 800; }
.attached-uses { background: var(--steel-deep); }
.attached-damage { background: var(--crimson-deep); }
@media (max-width: 760px) {
  .chips { display: none; }
  .attached li { min-height: 44px; font-size: 0.8rem; }
  .attached-name { white-space: normal; }
}
</style>
