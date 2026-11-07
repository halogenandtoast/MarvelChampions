<script setup lang="ts">
// The facets, on the right: type and aspect first, then the long tails — packs, sets and
// traits — which are searchable and truncated.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import FilterGroup from './FilterGroup.vue'
import { useCardStore } from '@browser/stores/cards'
import { ENCOUNTER_TYPES, FACTIONS, PLAYER_TYPES } from '@browser/types/card'
import type { FilterOption } from '@browser/types/facets'

const store = useCardStore()
const { t, te } = useI18n()
const label = (group: string, value: string) => (te(`browser.${group}.${value}`) ? t(`browser.${group}.${value}`) : value)

const typeOptions = computed<FilterOption[]>(() => {
  const counts = store.facets.types
  const ordered: string[] = [...PLAYER_TYPES, ...ENCOUNTER_TYPES]
  const extras = [...counts.keys()].filter((v) => !ordered.includes(v))
  return [...ordered, ...extras].filter((v) => counts.has(v)).map((value) => ({ value, label: label('cardTypes', value), count: counts.get(value) }))
})
const factionOptions = computed<FilterOption[]>(() =>
  FACTIONS.filter((f) => store.facets.factions.has(f)).map((value) => ({ value, label: t(`aspects.${value}`), count: store.facets.factions.get(value) })),
)
const packOptions = computed<FilterOption[]>(() => store.packs.map((p) => ({ value: p.code, label: p.name, count: store.facets.packs.get(p.code) ?? 0 })).filter((o) => o.count))
const setOptions = computed<FilterOption[]>(() => store.facets.sets.filter(([, set]) => set.type !== 'nemesis').map(([value, { name, count }]) => ({ value, label: name, count })))
const nemesisOptions = computed<FilterOption[]>(() => store.facets.sets.filter(([, set]) => set.type === 'nemesis').map(([value, { name, count }]) => ({ value, label: name, count })))
const traitOptions = computed<FilterOption[]>(() => store.facets.traits.map(([value, count]) => ({ value, label: value, count })))

// `v-model.number` leaves an empty input as '', neither a bound nor none
const costBound = (key: 'costMin' | 'costMax') =>
  computed({
    get: () => store.filters[key],
    set: (value: number | string | null) => {
      store.filters[key] = typeof value === 'number' && Number.isFinite(value) ? value : null
    },
  })
const costMin = costBound('costMin')
const costMax = costBound('costMax')
</script>

<template>
  <aside class="sidebar" :aria-label="$t('browser.filters.title')">
    <div class="head">
      <h2 class="title">{{ $t('browser.filters.title') }}</h2>
      <button v-if="store.filterCount" type="button" class="button button--ghost clear" @click="store.resetFilters()">
        {{ $t('browser.common.clearCount', { count: store.filterCount }) }}
      </button>
    </div>

    <FilterGroup :title="$t('browser.filters.type')" :options="typeOptions" :selected="store.filters.types" :max-visible="8" @toggle="store.toggle('types', $event)" />
    <FilterGroup :title="$t('browser.filters.aspect')" :options="factionOptions" :selected="store.filters.factions" @toggle="store.toggle('factions', $event)">
      <template #option="{ option }">
        <span class="dot" :data-aspect="option.value" />{{ option.label }}
      </template>
    </FilterGroup>

    <section class="group group--cost">
      <h3 class="group__title">{{ $t('browser.filters.cost') }}</h3>
      <div class="cost-range">
        <label>
          <span class="visually-hidden">{{ $t('browser.filters.minimumCost') }}</span>
          <input v-model.number="costMin" class="input" type="number" min="0" max="99" :placeholder="$t('browser.filters.min')" />
        </label>
        <span aria-hidden="true">–</span>
        <label>
          <span class="visually-hidden">{{ $t('browser.filters.maximumCost') }}</span>
          <input v-model.number="costMax" class="input" type="number" min="0" max="99" :placeholder="$t('browser.filters.max')" />
        </label>
      </div>
      <label class="check">
        <input v-model="store.filters.uniqueOnly" type="checkbox" />
        {{ $t('browser.filters.uniqueOnly') }}
      </label>
      <label class="check">
        <input v-model="store.showHidden" type="checkbox" />
        {{ $t('browser.filters.showBacks') }}
      </label>
    </section>

    <FilterGroup :title="$t('browser.filters.pack')" :options="packOptions" :selected="store.filters.packs" searchable :max-visible="10" @toggle="store.toggle('packs', $event)" />
    <FilterGroup :title="$t('browser.filters.set')" :options="setOptions" :selected="store.filters.sets" searchable :max-visible="10" :open="false" @toggle="store.toggle('sets', $event)" />
    <FilterGroup :title="$t('browser.filters.nemesisSets')" :options="nemesisOptions" :selected="store.filters.sets" searchable :max-visible="10" :open="false" @toggle="store.toggle('sets', $event)" />
    <FilterGroup :title="$t('browser.filters.traits')" :options="traitOptions" :selected="store.filters.traits" searchable :max-visible="10" :open="false" @toggle="store.toggle('traits', $event)" />
  </aside>
</template>

<style scoped>
.sidebar { display: flex; flex-direction: column; height: 100%; overflow-y: auto; scrollbar-gutter: stable; overscroll-behavior: contain; background: var(--c-surface); border-left: 1px solid var(--c-border); }
.head { display: flex; align-items: center; justify-content: space-between; gap: var(--space-2); padding: var(--space-3) var(--space-4); border-bottom: 1px solid var(--c-border); position: sticky; top: 0; background: var(--c-surface); z-index: 1; }
.title { font-size: 1.1rem; text-transform: uppercase; }
.clear { font-size: var(--text-xs); padding: 0.125rem 0.5rem; color: var(--c-accent); }
.group--cost { padding: var(--space-3) var(--space-4); border-bottom: 1px solid var(--c-border); }
.group__title { margin-bottom: var(--space-2); font-family: var(--font-ui); color: var(--c-text-muted); font-size: var(--text-xs); font-weight: 600; letter-spacing: 0.06em; text-transform: uppercase; }
.cost-range { display: grid; grid-template-columns: 1fr auto 1fr; align-items: center; gap: var(--space-2); color: var(--c-text-faint); }
.cost-range .input { text-align: center; appearance: textfield; }
.check { display: flex; align-items: center; gap: var(--space-2); margin-top: var(--space-3); font-size: var(--text-sm); cursor: pointer; }
.check input { accent-color: var(--c-accent); }
.dot { width: 0.6em; height: 0.6em; border-radius: 50%; background: var(--chip); flex: none; }
</style>
