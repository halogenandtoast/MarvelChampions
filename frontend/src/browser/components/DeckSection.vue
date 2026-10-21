<script setup lang="ts">
// One counted list of cards: a card type within the deck. A slot whose card did not
// resolve still gets a line, showing the code the source used.
import { computed } from 'vue'
import GameIcon from './GameIcon.vue'
import { useCardOverlay } from '../../composables/useCardOverlay'
import type { Slot } from '@browser/types/deck'

const { title, slots } = defineProps<{ title: string; slots: Slot[] }>()
const total = computed(() => slots.reduce((n, slot) => n + slot.quantity, 0))
const overlay = useCardOverlay()
const resources = (slot: Slot) => Object.entries(slot.card?.resources ?? {}).flatMap(([name, n]) => Array.from({ length: n ?? 0 }, () => name))
</script>

<template>
  <section class="section">
    <h3 class="title">{{ title }} <span class="count">{{ total }}</span></h3>
    <ul class="slots">
      <li v-for="slot in slots" :key="slot.code" class="slot" :data-aspect="slot.card?.faction ?? undefined">
        <span class="quantity">{{ slot.quantity }}×</span>
        <template v-if="slot.card">
          <RouterLink
            class="name"
            :to="`/cards/${slot.card.code}`"
            @pointerenter="$event.pointerType === 'mouse' && overlay.show({ code: slot.code, name: slot.card.name }, $event.currentTarget as HTMLElement)"
            @pointerleave="overlay.hide()"
          >
            <span class="dot" />
            <span class="truncate">{{ slot.card.name }}</span>
          </RouterLink>
          <span class="resources"><GameIcon v-for="(r, i) in resources(slot)" :key="i" :name="r" /></span>
          <span class="cost">{{ slot.card.cost ?? '' }}</span>
        </template>
        <template v-else>
          <span class="name unknown">{{ $t('browser.common.unknownCard') }} <code>{{ slot.code }}</code></span>
          <span />
          <span />
        </template>
      </li>
    </ul>
  </section>
</template>

<style scoped>
.title { display: flex; align-items: baseline; gap: var(--space-2); margin: 0; padding-bottom: var(--space-1); border-bottom: 1px solid var(--c-border); font-size: 1.2rem; text-transform: uppercase; }
.count { color: var(--c-text-faint); font-family: var(--font-ui); font-size: var(--text-xs); font-variant-numeric: tabular-nums; }
.slots { margin-top: var(--space-2); }
.slot { display: grid; grid-template-columns: 1.9rem minmax(0, 1fr) auto 1.4rem; align-items: center; gap: var(--space-2); padding: 0.2rem 0; font-size: var(--text-sm); }
.quantity { color: var(--c-text-faint); font-size: var(--text-xs); font-variant-numeric: tabular-nums; text-align: right; }
.name { display: flex; align-items: center; gap: 7px; min-width: 0; }
.name:hover { color: var(--title); }
.dot { flex: none; width: 7px; height: 7px; border-radius: 50%; background: var(--chip, var(--basic)); }
.unknown { color: var(--c-danger); }
.resources { display: flex; gap: 1px; }
.cost { color: var(--c-text-muted); font-family: var(--font-display); font-weight: 700; text-align: right; }
code { font-family: var(--font-mono); font-size: 0.9em; }
</style>
