<script setup lang="ts">
/**
 * The virtualised results list.
 *
 * An unfiltered browse is four thousand rows; rendering them all costs
 * seconds and hundreds of megabytes. Only the visible window is mounted. Row
 * heights are computed rather than measured — from the row height in list
 * mode, from the tile width and card orientation in grid mode — so the list
 * never has to lay a row out to find out how tall it is.
 */
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { useVirtualList } from '@vueuse/core'
import CardRow from './CardRow.vue'
import CardTile from './CardTile.vue'
import type { Card } from '@browser/types/card'

const { cards, mode, selectedCode } = defineProps<{
  cards: Card[]
  mode: 'list' | 'grid'
  selectedCode?: string | null
}>()

const emit = defineEmits<{ select: [card: Card] }>()

const ROW_HEIGHT = 56
const TILE_MIN_WIDTH = 170
const TILE_GAP = 12
const GRID_PAD_X = 12
/**
 * Everything in a tile that is not the card art: the gap under it, two lines
 * of caption, and the row's vertical padding. Keep in step with the styles at
 * the bottom of this file and with CardTile.
 */
const TILE_CAPTION = 8 + 31 + 16

const columns = ref(1)
const tileWidth = ref(TILE_MIN_WIDTH)

/**
 * The grid virtualises by row, so the item list has to be chunked into rows
 * of however many tiles currently fit.
 */
const rows = computed<Card[][]>(() => {
  if (mode === 'list') return cards.map((card) => [card])
  const perRow = Math.max(1, columns.value)
  const chunked: Card[][] = []
  for (let i = 0; i < cards.length; i += perRow) chunked.push(cards.slice(i, i + perRow))
  return chunked
})

/**
 * Schemes are printed landscape, and a row of them is barely
 * half the height of a row of portrait cards. Measuring each row against its
 * own tallest card avoids the dead space a single fixed height would leave
 * under every landscape row.
 */
function rowHeight(index: number): number {
  if (mode === 'list') return ROW_HEIGHT
  const row = rows.value[index]
  if (!row?.length) return ROW_HEIGHT
  const portrait = row.some((card) => card.orientation !== 'landscape')
  const art = portrait ? (tileWidth.value * 88) / 63 : (tileWidth.value * 63) / 88
  return Math.ceil(art) + TILE_CAPTION
}

const { list, containerProps, wrapperProps, scrollTo } = useVirtualList(rows, {
  itemHeight: rowHeight,
  overscan: 4,
})

// Recompute the column count from the real width rather than a media query,
// so the grid tracks the sidebar opening and closing too.
//
// The scroll container is the one useVirtualList measures, so we observe its
// ref rather than binding a second one — `v-bind="containerProps"` and a
// `ref` attribute on the same element would fight over it, and the template
// ref wins, leaving the virtual list with nothing to measure and no rows.
function measure(el: HTMLElement) {
  const inner = el.clientWidth - GRID_PAD_X * 2
  const n = Math.max(1, Math.floor((inner + TILE_GAP) / (TILE_MIN_WIDTH + TILE_GAP)))
  columns.value = n
  tileWidth.value = (inner - TILE_GAP * (n - 1)) / n
}

let observer: ResizeObserver | null = null

watch(
  containerProps.ref,
  (el) => {
    observer?.disconnect()
    if (!(el instanceof HTMLElement)) return
    measure(el)
    observer = new ResizeObserver(() => measure(el))
    observer.observe(el)
  },
  { immediate: true },
)

onBeforeUnmount(() => observer?.disconnect())

// A new search should start at the top, not wherever the last one left off.
watch(
  () => cards,
  () => scrollTo(0),
)

defineExpose({ scrollTo })
</script>

<template>
  <div v-bind="containerProps" class="list" :class="`list--${mode}`" role="list">
    <div v-bind="wrapperProps">
      <div
        v-for="row in list"
        :key="row.index"
        class="list__row"
        :style="{ height: `${rowHeight(row.index)}px` }"
      >
        <template v-if="mode === 'list'">
          <button
            v-for="card in row.data"
            :key="card.code"
            type="button"
            class="list__button"
            role="listitem"
            @click="emit('select', card)"
          >
            <CardRow :card="card" :selected="card.code === selectedCode" />
          </button>
        </template>

        <div v-else class="grid" :style="{ '--columns': columns }">
          <button
            v-for="card in row.data"
            :key="card.code"
            type="button"
            class="grid__button"
            role="listitem"
            @click="emit('select', card)"
          >
            <CardTile :card="card" :selected="card.code === selectedCode" />
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.list {
  height: 100%;
  overflow-y: auto;
  scrollbar-gutter: stable;
  overscroll-behavior: contain;
}

.list__row {
  contain: strict;
}

.list__button {
  display: block;
  width: 100%;
  height: 100%;
  text-align: left;
}

.list--grid .list__row {
  padding: 0 var(--space-3);
}

.grid {
  display: grid;
  grid-template-columns: repeat(var(--columns), minmax(0, 1fr));
  gap: var(--space-3);
  height: 100%;
  padding-block: var(--space-2);
}

.grid__button {
  min-width: 0;
  text-align: left;
}
</style>
