<script setup lang="ts">
// Entries sharing a group id, drawn as one block: the header names it for its whole life,
// members accumulate under it, and the summary closes it with the outcome. Any of the
// three may be missing, as an undo deletes by step.
import { computed } from 'vue'
import { Undo2 } from 'lucide-vue-next'
import LogPart from './LogPart.vue'
import GameLogEntry from './GameLogEntry.vue'
import { logPartsText, type LogGroupItem } from '../gameLog'

const props = defineProps<{
  group: LogGroupItem
  path: string
  isOpen: (path: string) => boolean
  isTail: (path: string) => boolean
  isPinned: (path: string) => boolean
  canUndo?: boolean
}>()

const emit = defineEmits<{
  toggle: [path: string, open: boolean]
  undo: [step: number, label: string]
}>()

const header = computed(() => props.group.header)
const band = computed(() => props.group.summary)
const tone = computed(() => band.value?.tone?.toLowerCase() ?? header.value?.tone?.toLowerCase() ?? 'neutral')

// A summary's body is its sentence and then the numbers, as their own stripe.
const bandSentence = computed(() => (band.value && band.value.body.length > 1 ? band.value.body.slice(0, -1) : (band.value?.body ?? [])))
const bandStats = computed(() => (band.value && band.value.body.length > 1 ? band.value.body[band.value.body.length - 1] : null))

const hasDetail = computed(() => props.group.members.length > 0)
const open = computed(() => hasDetail.value && props.isOpen(props.path))

// Back to before the block began: its header is written first.
const opening = computed(() => header.value ?? band.value)
const undoStep = computed(() => (props.canUndo && opening.value?.step != null ? opening.value.step : null))

const toggle = () => {
  if (hasDetail.value) emit('toggle', props.path, !open.value)
}

const requestUndo = () => {
  if (undoStep.value !== null && opening.value) emit('undo', undoStep.value, logPartsText(opening.value.body))
}
</script>

<template>
  <li class="log-group" :class="[`log-group--${tone}`, { 'log-entry--tail': isTail(path) }]">
    <component
      :is="hasDetail ? 'button' : 'div'"
      v-if="header"
      class="log-group__header"
      :type="hasDetail ? 'button' : undefined"
      :aria-expanded="hasDetail ? open : undefined"
      @click="toggle"
    >
      <span v-if="hasDetail" class="log-caret" :class="{ 'log-caret--open': open }">&#9654;</span>
      <span v-else class="log-caret-spacer" />
      <span class="log-group__body"><LogPart v-for="(part, i) in header.body" :key="i" :part="part" /></span>
    </component>

    <ul v-if="open" class="log-group__detail">
      <template v-for="(member, i) in group.members" :key="member.kind === 'group' ? `g${member.id}` : `e${i}`">
        <!-- a block inside a block: the shadow card an attack turns up -->
        <LogGroupBlock
          v-if="member.kind === 'group'"
          :group="member"
          :path="`${path}.${i}`"
          :is-open="isOpen"
          :is-tail="isTail"
          :is-pinned="isPinned"
          @toggle="(p, o) => emit('toggle', p, o)"
        />
        <GameLogEntry
          v-else
          :entry="member.entry"
          :path="`${path}.${i}`"
          :depth="1"
          :is-open="isOpen"
          :is-tail="isTail"
          :is-pinned="isPinned"
          @toggle="(p, o) => emit('toggle', p, o)"
        />
      </template>
    </ul>

    <component
      :is="hasDetail ? 'button' : 'div'"
      v-if="band"
      class="log-group__band"
      :type="hasDetail ? 'button' : undefined"
      :aria-expanded="hasDetail ? open : undefined"
      @click="toggle"
    >
      <span class="log-group__line">
        <!-- the caret belongs to the block's first bar, which is this one without a header -->
        <span v-if="hasDetail && !header" class="log-caret" :class="{ 'log-caret--open': open }">&#9654;</span>
        <span v-else class="log-caret-spacer" />
        <span class="log-group__body"><LogPart v-for="(part, i) in bandSentence" :key="i" :part="part" /></span>
      </span>
      <span v-if="bandStats" class="log-group__stats"><LogPart :part="bandStats" /></span>
    </component>

    <button v-if="undoStep !== null" class="log-undo" type="button" :title="$t('log.undoToHere')" :aria-label="$t('log.undoToHere')" @click.stop="requestUndo">
      <Undo2 aria-hidden="true" />
    </button>
  </li>
</template>

<style scoped>
.log-group {
  --tone: var(--line-strong);
  position: relative;
  list-style: none;
  margin: 6px 0;
  border: 1px solid var(--line);
  border-left: 3px solid var(--tone);
  border-radius: 3px 5px 5px 3px;
  background: rgb(255 255 255 / 0.03);
  overflow: hidden;
  color: var(--text);
  line-height: 1.45;
}
.log-group--good { --tone: var(--success); }
.log-group--bad { --tone: var(--danger); }

.log-group__header,
.log-group__band {
  display: block;
  width: 100%;
  padding: 0;
  font: inherit;
  color: inherit;
  text-align: left;
  border: 0;
  border-radius: 0;
}
.log-group__header {
  display: flex;
  gap: 6px;
  align-items: baseline;
  padding: 5px 24px 5px 7px;
  background: rgb(255 255 255 / 0.05);
}
.log-group__header + .log-group__detail,
.log-group__detail + .log-group__band,
.log-group__header + .log-group__band { border-top: 1px solid var(--line); }
button.log-group__header,
button.log-group__band { cursor: pointer; }
button.log-group__header:hover { background: rgb(255 255 255 / 0.09); }
button.log-group__header:focus-visible,
button.log-group__band:focus-visible { outline: 2px solid var(--accent); outline-offset: -2px; }

.log-group__detail { margin: 0; padding: 5px 8px 5px 10px; display: flex; flex-direction: column; gap: 2px; }
/* a block inside a block sits tighter, a touch lighter */
.log-group__detail > .log-group { margin: 3px 0; background: rgb(255 255 255 / 0.05); }

.log-group__band { background: color-mix(in srgb, var(--tone) 28%, transparent); }
button.log-group__band:hover { background: color-mix(in srgb, var(--tone) 40%, transparent); }
.log-group__line { display: flex; gap: 6px; align-items: baseline; padding: 5px 24px 5px 7px; }
.log-group__body { flex: 1 1 auto; min-width: 0; }
.log-group__stats {
  display: block;
  padding: 3px 8px 3px 22px;
  border-top: 1px solid var(--line);
  background: rgb(0 0 0 / 0.24);
  font-size: 0.92em;
  font-variant-numeric: tabular-nums;
  color: var(--text-muted);
}

.log-caret { flex: none; width: 9px; font-size: 0.6em; color: var(--accent); transition: transform 140ms ease; }
.log-caret--open { transform: rotate(90deg); }
.log-caret-spacer { flex: none; width: 9px; }

.log-undo {
  position: absolute;
  top: 3px;
  right: 2px;
  display: flex;
  align-items: center;
  justify-content: center;
  width: 20px;
  height: 20px;
  padding: 0;
  color: var(--text-muted);
  background: rgb(0 0 0 / 0.55);
  border: 1px solid var(--line);
  border-radius: 4px;
  cursor: pointer;
  opacity: 0;
  transition: opacity 120ms ease;
}
.log-undo svg { width: 12px; height: 12px; }
.log-group:hover > .log-undo,
.log-undo:focus-visible { opacity: 1; }
.log-undo:hover { color: var(--text); border-color: var(--accent); }

@media (hover: none) {
  .log-undo { opacity: 0.6; }
}
@media (prefers-reduced-motion: reduce) {
  .log-caret, .log-undo { transition: none; }
}
</style>
