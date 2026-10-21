<script setup lang="ts">
// One log entry, with its detail folded underneath. Recursive: children render as this
// component one level down, keyed by index path ("7.0.1"), which is also the key for
// whether it is open. That state lives in GameLog.vue; this is told and reports clicks.
import { computed } from 'vue'
import { Undo2 } from 'lucide-vue-next'
import LogPart from './LogPart.vue'
import { logPartsText, type LogEntry } from '../gameLog'

const props = defineProps<{
  entry: LogEntry
  path: string
  depth: number
  isOpen: (path: string) => boolean
  isTail: (path: string) => boolean
  isPinned: (path: string) => boolean
  canUndo?: boolean
}>()

const emit = defineEmits<{
  toggle: [path: string, open: boolean]
  undo: [step: number, label: string]
}>()

// Headings take their tone from their key: "log.phase.villain" is a villain phase banner.
const structureTone = computed(() => {
  const first = props.entry.body[0]
  if (first?.tag !== 'LogI18n') return null
  const key = first.contents[0]
  if (key.startsWith('log.phase.')) return key.slice('log.phase.'.length)
  if (key === 'log.scenarioBegins') return 'scenario'
  if (key === 'log.round') return 'round'
  if (key.startsWith('log.gameOver.')) return 'over'
  return null
})

// The scenario, the round and the phase stay pinned atop the log, stacked in that order,
// each until the next of its kind scrolls up to replace it.
const phases = ['player', 'villain']
const stickyTier = computed(() => {
  const tone = structureTone.value
  if (tone === 'scenario' || tone === 'round') return tone
  return tone && phases.includes(tone) ? 'phase' : null
})

// A Test's body is its sentence and then the numbers, drawn apart as their own stripe.
const testSentence = computed(() => (props.entry.body.length > 1 ? props.entry.body.slice(0, -1) : props.entry.body))
const testStats = computed(() => (props.entry.body.length > 1 ? props.entry.body[props.entry.body.length - 1] : null))

// Chat draws the speaker apart from the words: the body is "name", ": ", "words".
const chatSpeaker = computed(() => (props.entry.body.length > 2 ? props.entry.body[0] : null))
const chatWords = computed(() => (props.entry.body.length > 2 ? props.entry.body.slice(2) : props.entry.body))

// Leaf notices under an entry belong to its line rather than beneath it: always shown,
// no caret.
const annotated = computed(
  () => props.entry.children.length > 0 && props.entry.children.every((c) => c.kind === 'Notice' && c.children.length === 0),
)
const hasKids = computed(() => !annotated.value && props.entry.children.length > 0)
const open = computed(() => (annotated.value ? true : hasKids.value && props.isOpen(props.path)))

const toggle = () => {
  if (hasKids.value) emit('toggle', props.path, !open.value)
}

// Only a top-level entry offers an undo: a child shares its parent's step. Chat never
// does, as what is said survives an undo and its step only says where it now sits.
const undoStep = computed(() =>
  props.canUndo && props.depth === 0 && props.entry.step !== null && props.entry.kind !== 'Chat' ? props.entry.step : null,
)

const requestUndo = () => {
  if (undoStep.value !== null) emit('undo', undoStep.value, logPartsText(props.entry.body))
}
</script>

<template>
  <li
    v-if="entry.kind === 'Structure'"
    class="log-structure"
    :class="[
      structureTone ? `log-structure--${structureTone}` : null,
      depth > 0 ? 'log-structure--nested' : stickyTier ? `log-structure--pinned log-structure--pinned-${stickyTier}` : null,
    ]"
  >
    <LogPart v-for="(part, i) in entry.body" :key="i" :part="part" />
    <button v-if="undoStep !== null" class="log-undo" type="button" :title="$t('log.undoToHere')" :aria-label="$t('log.undoToHere')" @click.stop="requestUndo">
      <Undo2 aria-hidden="true" />
    </button>
  </li>

  <li
    v-else-if="entry.kind === 'Test' && depth === 0"
    class="log-entry log-entry--test"
    :class="[`log-test--${entry.tone?.toLowerCase() ?? 'neutral'}`, { 'log-entry--tail': isTail(path) }]"
  >
    <div class="log-test__band">
      <span class="log-test__body"><LogPart v-for="(part, i) in testSentence" :key="i" :part="part" /></span>
      <span v-if="testStats" class="log-test__stats"><LogPart :part="testStats" /></span>
    </div>
    <button v-if="undoStep !== null" class="log-undo" type="button" :title="$t('log.undoToHere')" :aria-label="$t('log.undoToHere')" @click.stop="requestUndo">
      <Undo2 aria-hidden="true" />
    </button>
  </li>

  <li v-else-if="entry.kind === 'Chat'" class="log-entry log-entry--chat">
    <div class="log-chat">
      <LogPart v-if="chatSpeaker" :part="chatSpeaker" class="log-chat__who" />
      <p class="log-chat__words"><LogPart v-for="(part, i) in chatWords" :key="i" :part="part" /></p>
    </div>
  </li>

  <li
    v-else
    class="log-entry"
    :class="[
      `log-entry--${entry.kind.toLowerCase()}`,
      entry.tone ? `log-entry--${entry.tone.toLowerCase()}` : null,
      { 'log-entry--nested': depth > 0, 'log-entry--tail': isTail(path) },
    ]"
  >
    <component
      :is="hasKids ? 'button' : 'div'"
      class="log-headline"
      :type="hasKids ? 'button' : undefined"
      :aria-expanded="hasKids ? open : undefined"
      @click="toggle"
    >
      <span v-if="hasKids" class="log-caret" :class="{ 'log-caret--open': open }">&#9654;</span>
      <span v-else class="log-caret-spacer" />
      <span class="log-body"><LogPart v-for="(part, i) in entry.body" :key="i" :part="part" /></span>
      <span v-if="hasKids && isPinned(path)" class="log-pin" :title="$t('log.pinned')" />
    </component>

    <!-- beside the headline, not in it: a button inside a button cannot be clicked -->
    <button v-if="undoStep !== null" class="log-undo" type="button" :title="$t('log.undoToHere')" :aria-label="$t('log.undoToHere')" @click.stop="requestUndo">
      <Undo2 aria-hidden="true" />
    </button>

    <ul v-if="open" class="log-children" :class="{ 'log-children--annotation': annotated }">
      <GameLogEntry
        v-for="(child, i) in entry.children"
        :key="i"
        :entry="child"
        :path="`${path}.${i}`"
        :depth="depth + 1"
        :is-open="isOpen"
        :is-tail="isTail"
        :is-pinned="isPinned"
        :can-undo="canUndo"
        @toggle="(p, o) => emit('toggle', p, o)"
        @undo="(st, l) => emit('undo', st, l)"
      />
    </ul>
  </li>
</template>

<style scoped>
/* A banner across the column, so a phase reads as a break rather than another line. */
.log-structure {
  --tone: var(--ink-5);
  position: relative;
  list-style: none;
  margin: 14px -0.6rem 5px;
  padding: 5px 0.6rem 5px calc(0.6rem - 3px);
  border-left: 3px solid var(--tone);
  background: color-mix(in srgb, var(--tone) 35%, transparent);
  font-size: 0.92em;
  font-weight: 700;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--text);
}
.log-structure--scenario:first-child { margin-top: 0; }
.log-structure--nested { margin: 4px -10px 1px -12px; padding: 4px 10px 4px 9px; }
/* the phase bar's own colours */
.log-structure--player { --tone: var(--phase-player); }
.log-structure--villain { --tone: var(--phase-villain); }
/* the scenario's title opens the whole log: the one banner that is big */
.log-structure--scenario {
  --tone: var(--crimson);
  padding: 10px 0.6rem;
  font-family: var(--font-title);
  font-size: 1.3em;
  font-weight: 400;
  letter-spacing: 0.04em;
  text-transform: none;
  text-align: center;
  color: var(--title);
}
.log-structure--round { --tone: var(--line-strong); margin-top: 22px; font-size: 1em; }
.log-structure--pinned {
  position: sticky;
  display: flex;
  align-items: center;
  box-sizing: border-box;
  white-space: nowrap;
  overflow: hidden;
  /* opaque, so what scrolls under it does not show through: the tone can itself be
     translucent, so it is laid over a solid surface rather than mixed into it */
  background: linear-gradient(color-mix(in srgb, var(--tone) 35%, transparent) 0 0), var(--log-surface);
  /* the gap before it belongs to the section above, so it lands flush under the
     banner it replaces before pushing it off */
  margin-top: 0;
}
.log-structure--pinned-scenario { top: 0; z-index: 3; height: var(--banner-scenario); padding-block: 0; justify-content: center; }
.log-structure--pinned-round { top: var(--banner-scenario); z-index: 2; height: var(--banner-round); padding-block: 0; }
.log-structure--pinned-phase {
  top: calc(var(--banner-scenario) + var(--banner-round));
  z-index: 1;
  height: var(--banner-phase);
  padding-block: 0;
}
.log-structure--over { --tone: var(--title); text-align: center; font-size: 1.05em; }

.log-entry {
  position: relative;
  list-style: none;
  margin: 0;
  color: var(--text);
  line-height: 1.45;
  border-bottom: 1px solid rgb(255 255 255 / 0.06);
}
.log-entry:last-child { border-bottom: 0; }
.log-entry--nested {
  margin: 3px 0;
  color: var(--text-muted);
  border-bottom: 0;
  border-left: 1px solid var(--line);
  padding-left: 8px;
}
.log-entry--tail { box-shadow: inset 2px 0 0 var(--accent); }
.log-entry--notice { color: var(--text-muted); }
.log-entry--good > .log-headline .log-body { color: #9fd8ae; }
.log-entry--bad > .log-headline .log-body { color: #f0a4a9; }

/* a contest decided by numbers: a band, coloured by how it went */
.log-entry--test {
  --tone: var(--line-strong);
  margin: 6px 0;
  border: 1px solid var(--line);
  border-left: 3px solid var(--tone);
  border-radius: 3px 5px 5px 3px;
  overflow: hidden;
}
.log-test--good { --tone: var(--success); }
.log-test--bad { --tone: var(--danger); }
.log-test__band {
  display: flex;
  flex-wrap: wrap;
  gap: 4px 6px;
  align-items: baseline;
  padding: 5px 8px;
  background: color-mix(in srgb, var(--tone) 28%, transparent);
}
.log-test__body { flex: 1 1 12em; min-width: 0; }
.log-test__stats {
  flex: none;
  align-self: center;
  margin-left: auto;
  padding: 1px 7px;
  font-size: 0.9em;
  font-variant-numeric: tabular-nums;
  background: rgb(0 0 0 / 0.3);
  border-radius: 9px;
  white-space: nowrap;
}

/* not a line of play at all, so it does not look like one */
.log-entry--chat { margin: 8px 0; border: 0; }
.log-chat {
  padding: 6px 9px 7px;
  background: rgb(255 255 255 / 0.06);
  border: 1px solid var(--line);
  border-left: 3px solid var(--accent);
  border-radius: 4px 6px 6px 4px;
}
.log-chat__who {
  display: inline-block;
  margin-bottom: 2px;
  font-size: 0.85em;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--title);
}
.log-chat__words { margin: 0; white-space: pre-wrap; overflow-wrap: anywhere; }

.log-headline {
  display: flex;
  gap: 6px;
  align-items: baseline;
  width: 100%;
  padding: 4px 22px 4px 0;
  font: inherit;
  color: inherit;
  text-align: left;
  background: none;
  border: 0;
  border-radius: 4px;
}
.log-entry--nested > .log-headline { padding: 0 22px 0 0; }
button.log-headline { cursor: pointer; }
button.log-headline:hover { background: rgb(255 255 255 / 0.05); }
button.log-headline:focus-visible { outline: 2px solid var(--accent); outline-offset: 1px; }

.log-caret { flex: none; width: 9px; font-size: 0.6em; color: var(--accent); transition: transform 140ms ease; }
.log-caret--open { transform: rotate(90deg); }
.log-caret-spacer { flex: none; width: 9px; }
.log-body { flex: 1 1 auto; min-width: 0; }
.log-pin { flex: none; align-self: center; width: 5px; height: 5px; border-radius: 50%; background: var(--accent-bright); }

/* hidden until the row is hovered or focused: every entry can rewind the game */
.log-undo {
  position: absolute;
  top: 2px;
  right: 0;
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
.log-entry:hover > .log-undo,
.log-structure:hover > .log-undo,
.log-undo:focus-visible { opacity: 1; }
.log-undo:hover { color: var(--text); border-color: var(--accent); }

.log-children { margin: 0; padding: 0 2px 6px 15px; display: flex; flex-direction: column; gap: 3px; }
.log-children--annotation { gap: 0; padding: 0 2px 3px 15px; }
.log-children--annotation :deep(.log-caret-spacer) { display: none; }
.log-entry--nested > .log-children { padding: 4px 0 0 12px; }

@media (hover: none) {
  .log-undo { opacity: 0.6; }
}
@media (prefers-reduced-motion: reduce) {
  .log-caret, .log-undo { transition: none; }
}
</style>
