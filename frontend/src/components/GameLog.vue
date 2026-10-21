<script setup lang="ts">
// The game log panel, as Arkham's GameLog.vue. It owns what is open, so the live-tail
// rule has one home: the newest entry with detail is open, and shuts when the next
// arrives; a block stays open until something outside it follows. A click pins an entry
// against the rule, so an older attack can be read while play goes on.
import { computed, nextTick, ref, watch } from 'vue'
import GameLogEntry from './GameLogEntry.vue'
import LogGroupBlock from './LogGroupBlock.vue'
import { groupLogEntries, type LogEntry, type LogItem } from '../gameLog'

const props = withDefaults(
  defineProps<{
    entries: readonly LogEntry[]
    /** Whether entries offer "undo back to here": not for a spectator. */
    canUndo?: boolean
    /** Whether there is a box to say something in. */
    canChat?: boolean
    /** Whether older entries are still to be had. */
    hasOlder?: boolean
    /** The scenario's name, for its banner while the entry that opens the log is unloaded. */
    scenario?: string | null
  }>(),
  { canUndo: false, canChat: false, hasOlder: false, scenario: null },
)

const emit = defineEmits<{
  undo: [step: number, label: string]
  say: [text: string]
  loadOlder: [beforeSeq: number]
}>()

const draft = ref('')
const send = () => {
  const text = draft.value.trim()
  if (!text) return
  draft.value = ''
  emit('say', text)
}

// The scenario's banner opens the log and stays pinned atop it, so stand one in until
// scrolling back reaches the real one.
const opener = (name: string): LogEntry => ({
  seq: 0,
  step: null,
  kind: 'Structure',
  body: [{ tag: 'LogI18n', contents: ['log.scenarioBegins', { villain: { tag: 'LogRefPart', contents: { kind: 'RefVillain', name, cardCode: null, cardId: null, entityId: null, faceDown: false } } }] }],
  source: null,
  children: [],
  audience: { tag: 'Everyone' },
  context: null,
  tone: null,
  group: null,
})
const items = computed(() =>
  groupLogEntries(props.hasOlder && props.scenario ? [opener(props.scenario), ...props.entries] : props.entries),
)

// Each round's entries, and each phase's within it, are an element of their own, so a
// pinned banner stays only while its own round or phase is in view and is pushed off by
// the next. Items keep their index, which is their path.
type Indexed = { item: LogItem; i: number }
type PhaseSection = { banner: Indexed | null; items: Indexed[] }
type RoundSection = { banner: Indexed | null; phases: PhaseSection[] }

const headingKey = (item: LogItem) => {
  if (item.kind !== 'entry' || item.entry.kind !== 'Structure') return null
  const first = item.entry.body[0]
  return first?.tag === 'LogI18n' ? first.contents[0] : null
}

const sections = computed(() => {
  const opening: Indexed[] = []
  const rounds: RoundSection[] = []
  items.value.forEach((item, i) => {
    const key = headingKey(item)
    if (key === 'log.round') {
      rounds.push({ banner: { item, i }, phases: [] })
      return
    }
    const round = rounds[rounds.length - 1]
    if (!round) {
      opening.push({ item, i })
      return
    }
    if (key?.startsWith('log.phase.')) {
      round.phases.push({ banner: { item, i }, items: [] })
      return
    }
    if (!round.phases.length) round.phases.push({ banner: null, items: [] })
    round.phases[round.phases.length - 1].items.push({ item, i })
  })
  return { opening, rounds }
})

const keyOf = ({ item }: Indexed) => (item.kind === 'group' ? `g${item.id}` : `e${item.entry.seq}`)
const componentOf = ({ item }: Indexed) => (item.kind === 'entry' ? GameLogEntry : LogGroupBlock)
const propsOf = ({ item, i }: Indexed) => ({
  ...(item.kind === 'entry' ? { entry: item.entry, depth: 0 } : { group: item }),
  path: String(i),
  isOpen,
  isTail,
  isPinned,
  canUndo: props.canUndo,
})

// The newest top-level entry with detail to show. Indexed into items, as paths are.
const tailPath = computed(() => {
  for (let i = items.value.length - 1; i >= 0; i -= 1) {
    const item = items.value[i]
    if (item.kind === 'entry' && item.entry.children.length > 0) return String(i)
  }
  return null
})

// Kept only where a click differs from the rule, so a pin lapses once they agree.
const pinnedOpen = ref(new Set<string>())
const pinnedShut = ref(new Set<string>())

const autoOpen = (path: string) => {
  const top = path.split('.')[0]
  const item = items.value[Number(top)]
  // Open until something outside it follows, chat aside. Not until its summary: an
  // attack's damage lands after it is decided, and closing then hid it as it arrived.
  if (item?.kind === 'group') {
    return !items.value.slice(Number(top) + 1).some((next) => next.kind === 'group' || next.entry.kind !== 'Chat')
  }
  return tailPath.value !== null && top === tailPath.value
}

const isOpen = (path: string) => (pinnedShut.value.has(path) ? false : pinnedOpen.value.has(path) || autoOpen(path))
const isTail = (path: string) => path === tailPath.value
const isPinned = (path: string) => pinnedOpen.value.has(path) || pinnedShut.value.has(path)

const onToggle = (path: string, open: boolean) => {
  const opened = new Set(pinnedOpen.value)
  const shut = new Set(pinnedShut.value)
  opened.delete(path)
  shut.delete(path)
  if (open !== autoOpen(path)) (open ? opened : shut).add(path)
  pinnedOpen.value = opened
  pinnedShut.value = shut
}

const scroller = ref<HTMLElement | null>(null)
const pinnedToBottom = ref(true)
const loadingOlder = ref(false)

// Follow the log only for a reader already at the bottom; near the top, ask for the
// page before what is held.
const onScroll = () => {
  const el = scroller.value
  if (!el) return
  pinnedToBottom.value = el.scrollHeight - el.scrollTop - el.clientHeight < 40
  const oldest = props.entries[0]?.seq
  if (el.scrollTop < 80 && props.hasOlder && !loadingOlder.value && oldest !== undefined) {
    loadingOlder.value = true
    emit('loadOlder', oldest)
  }
}

// Once older entries land, keep the reader where they were, as the content above them
// just grew.
watch(
  () => props.entries[0]?.seq,
  async () => {
    if (!loadingOlder.value) return
    const el = scroller.value
    const heightBefore = el?.scrollHeight ?? 0
    const topBefore = el?.scrollTop ?? 0
    await nextTick()
    if (el) el.scrollTop = topBefore + el.scrollHeight - heightBefore
    loadingOlder.value = false
  },
)
watch(
  () => props.hasOlder,
  (more) => {
    if (!more) loadingOlder.value = false
  },
)

watch(
  () => props.entries[props.entries.length - 1]?.seq ?? 0,
  async () => {
    if (!pinnedToBottom.value) return
    await nextTick()
    const el = scroller.value
    if (el) el.scrollTop = el.scrollHeight
  },
  { immediate: true, flush: 'post' },
)
</script>

<template>
  <div class="game-log">
    <div ref="scroller" class="game-log__scroll" @scroll.passive="onScroll">
      <p v-if="!entries.length" class="game-log__empty">{{ $t('log.empty') }}</p>
      <ul class="game-log__list">
        <component
          :is="componentOf(x)"
          v-for="x in sections.opening"
          :key="keyOf(x)"
          v-bind="propsOf(x)"
          @toggle="onToggle"
          @undo="(step: number, label: string) => emit('undo', step, label)"
        />
        <li
          v-for="(round, r) in sections.rounds"
          :key="round.banner ? keyOf(round.banner) : 'round'"
          class="game-log__section game-log__section--round"
          :class="{ 'game-log__section--first': r === 0 }"
        >
          <ul class="game-log__list">
            <component
              :is="componentOf(round.banner)"
              v-if="round.banner"
              v-bind="propsOf(round.banner)"
              @undo="(step: number, label: string) => emit('undo', step, label)"
            />
            <li v-for="(phase, p) in round.phases" :key="phase.banner ? keyOf(phase.banner) : `p${p}`" class="game-log__section game-log__section--phase">
              <ul class="game-log__list">
                <component
                  :is="componentOf(phase.banner)"
                  v-if="phase.banner"
                  v-bind="propsOf(phase.banner)"
                  @undo="(step: number, label: string) => emit('undo', step, label)"
                />
                <component
                  :is="componentOf(x)"
                  v-for="x in phase.items"
                  :key="keyOf(x)"
                  v-bind="propsOf(x)"
                  @toggle="onToggle"
                  @undo="(step: number, label: string) => emit('undo', step, label)"
                />
              </ul>
            </li>
          </ul>
        </li>
      </ul>
    </div>

    <form v-if="canChat" class="game-log__compose" @submit.prevent="send">
      <input
        v-model="draft"
        class="game-log__input"
        type="text"
        maxlength="500"
        :placeholder="$t('log.sayPlaceholder')"
        :aria-label="$t('log.sayPlaceholder')"
      />
      <button class="game-log__send" type="submit" :disabled="draft.trim().length === 0">{{ $t('log.say') }}</button>
    </form>
  </div>
</template>

<style scoped>
.game-log {
  flex: 1;
  min-height: 6rem;
  display: flex;
  flex-direction: column;
  background: var(--ink-2);
  border: 1px solid var(--line);
  border-radius: 12px;
  overflow: hidden;
}
.game-log__scroll {
  /* the pinned banners' heights, which is where each stacks under the last */
  --banner-scenario: 34px;
  --banner-round: 24px;
  --banner-phase: 24px;
  --log-surface: var(--ink-2);
  flex: 1 1 auto;
  min-height: 0;
  overflow-y: auto;
  overflow-x: hidden;
  /* none on top: a pinned banner sticks to the edge, not under a gap */
  padding: 0 0.6rem 0.5rem;
  font-size: 0.72rem;
}
.game-log__list { list-style: none; margin: 0; padding: 0; }
.game-log__empty { margin: 1.5rem 0; text-align: center; color: var(--text-faint); font-style: italic; }
.game-log__section { list-style: none; }
/* The gap after a round or phase is inside it, as the end of its content: a banner is
   held only within its section, so a gap between sections would push it off while the
   next banner was still a gap away. */
.game-log__section--phase > .game-log__list::after { content: ''; display: block; height: 14px; }
/* a round's gap is its last phase's, so the round and that phase end together */
.game-log__section--phase:last-child > .game-log__list::after { height: 22px; }
.game-log__section--first { margin-top: 14px; }
.game-log__compose {
  flex: none;
  display: flex;
  gap: 6px;
  padding: 6px;
  border-top: 1px solid var(--line);
  background: rgb(0 0 0 / 0.2);
}
.game-log__input {
  flex: 1 1 auto;
  min-width: 0;
  padding: 5px 8px;
  font: inherit;
  font-size: 0.75rem;
  color: var(--text);
  background: rgb(0 0 0 / 0.3);
  border: 1px solid var(--line);
  border-radius: 4px;
}
.game-log__input::placeholder { color: var(--cream-faint); }
.game-log__input:focus-visible { outline: 2px solid var(--accent); outline-offset: -1px; }
.game-log__send {
  flex: none;
  padding: 5px 10px;
  font: inherit;
  font-size: 0.7rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--text-muted);
  background: rgb(255 255 255 / 0.07);
  border: 1px solid var(--line);
  border-radius: 4px;
  cursor: pointer;
}
.game-log__send:hover:not(:disabled) { color: var(--text); background: rgb(255 255 255 / 0.13); }
.game-log__send:disabled { opacity: 0.4; cursor: default; }
</style>
