<script setup lang="ts">
/**
 * A collapsible facet in the sidebar: a list of checkable values with counts,
 * optionally searchable when there are too many to scan.
 */
import { computed, ref } from 'vue'
import { normalize } from '@browser/lib/search'
import type { FilterOption } from '@browser/types/facets'

const {
  title,
  options,
  selected,
  searchable = false,
  searchPlaceholder,
  open: initiallyOpen = true,
  maxVisible = 0,
} = defineProps<{
  title: string
  options: FilterOption[]
  selected: readonly string[]
  searchable?: boolean
  searchPlaceholder?: string
  open?: boolean
  /** Collapse the list past this many options behind a "show all" toggle. */
  maxVisible?: number
}>()

const emit = defineEmits<{ toggle: [value: string] }>()

const open = ref(initiallyOpen)
const query = ref('')
const expanded = ref(false)

const filtered = computed(() => {
  const needle = normalize(query.value)
  if (!needle) return options
  return options.filter((o) => normalize(o.label).includes(needle))
})

/** Anything already chosen stays visible even when the list is truncated. */
const visible = computed(() => {
  if (expanded.value || query.value || maxVisible === 0) return filtered.value
  const head = filtered.value.slice(0, maxVisible)
  const chosen = filtered.value.filter((o) => selected.includes(o.value) && !head.includes(o))
  return [...head, ...chosen]
})

const hiddenCount = computed(() => filtered.value.length - visible.value.length)
const selectedCount = computed(() => selected.length)
</script>

<template>
  <section class="group">
    <button type="button" class="group__header" :aria-expanded="open" @click="open = !open">
      <span class="group__chevron" :class="{ 'group__chevron--open': open }" aria-hidden="true">
        &#9656;
      </span>
      <span class="group__title">{{ title }}</span>
      <span v-if="selectedCount" class="group__badge">{{ selectedCount }}</span>
    </button>

    <div v-if="open" class="group__body">
      <input
        v-if="searchable"
        v-model="query"
        class="input group__search"
        type="search"
        :placeholder="searchPlaceholder ?? $t('browser.filters.filterPlaceholder')"
        :aria-label="$t('browser.filters.searchGroup', { title })"
      />

      <ul class="options">
        <li v-for="option in visible" :key="option.value">
          <label class="option" :class="{ 'option--on': selected.includes(option.value) }">
            <input
              type="checkbox"
              :checked="selected.includes(option.value)"
              @change="emit('toggle', option.value)"
            />
            <span class="option__label truncate">
              <slot name="option" :option="option">{{ option.label }}</slot>
            </span>
            <span v-if="option.count !== undefined" class="option__count">{{ option.count }}</span>
          </label>
        </li>
        <li v-if="!visible.length" class="options__empty">{{ $t('browser.filters.noMatches') }}</li>
      </ul>

      <button v-if="hiddenCount > 0" type="button" class="group__more" @click="expanded = true">
        {{ $t('browser.filters.showMore', { count: hiddenCount }) }}
      </button>
      <button
        v-else-if="expanded && maxVisible > 0 && !query"
        type="button"
        class="group__more"
        @click="expanded = false"
      >
        {{ $t('browser.filters.showFewer') }}
      </button>
    </div>
  </section>
</template>

<style scoped>
.group {
  border-bottom: 1px solid var(--c-border);
}

.group__header {
  display: flex;
  align-items: center;
  gap: var(--space-2);
  width: 100%;
  padding: var(--space-3) var(--space-4);
  text-align: left;
  color: var(--c-text-muted);
  font-size: var(--text-xs);
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
}

.group__header:hover {
  color: var(--c-text);
}

.group__chevron {
  display: inline-block;
  transition: transform var(--transition);
  font-size: 0.85em;
}

.group__chevron--open {
  transform: rotate(90deg);
}

.group__title {
  flex: 1;
}

.group__badge {
  display: grid;
  place-items: center;
  min-width: 1.35em;
  height: 1.35em;
  padding: 0 0.35em;
  border-radius: 999px;
  background: var(--c-accent);
  color: var(--c-accent-contrast);
  font-size: 0.9em;
}

.group__body {
  padding: 0 var(--space-4) var(--space-3);
}

.group__search {
  margin-bottom: var(--space-2);
}

.options {
  display: flex;
  flex-direction: column;
  max-height: 17rem;
  overflow-y: auto;
}

.option {
  display: flex;
  align-items: center;
  gap: var(--space-2);
  padding: 0.1875rem var(--space-2);
  border-radius: var(--radius-sm);
  font-size: var(--text-sm);
  cursor: pointer;
}

.option:hover {
  background: var(--c-surface-2);
}

.option--on {
  color: var(--c-accent);
}

.option input {
  accent-color: var(--c-accent);
  flex: none;
  margin: 0;
}

.option__label {
  flex: 1;
  min-width: 0;
  display: flex;
  align-items: center;
  gap: 0.35em;
}

.option__count {
  color: var(--c-text-faint);
  font-size: var(--text-xs);
  font-variant-numeric: tabular-nums;
}

.options__empty {
  padding: var(--space-2);
  color: var(--c-text-faint);
  font-size: var(--text-sm);
}

.group__more {
  margin-top: var(--space-2);
  padding: 0 var(--space-2);
  color: var(--c-accent);
  font-size: var(--text-xs);
}

.group__more:hover {
  text-decoration: underline;
}
</style>
