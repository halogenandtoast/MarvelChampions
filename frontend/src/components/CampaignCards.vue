<script setup lang="ts">
import { computed, onBeforeUnmount } from 'vue'
import { useCards } from '../cards'
import { useCardOverlay } from '../composables/useCardOverlay'

const props = defineProps<{ codes: readonly (string | null | undefined)[] }>()
const codes = computed(() => props.codes.filter((code): code is string => !!code))
const cards = useCards()
const overlay = useCardOverlay()
const preview = (code: string, element: EventTarget | null, pinned = false) => {
  if (element instanceof HTMLElement) overlay.show({ code, name: cards.name(code, code) }, element, undefined, pinned)
}
onBeforeUnmount(overlay.hide)
</script>

<template>
  <span class="campaign-cards">
    <template v-for="(code, index) in codes" :key="`${code}-${index}`">
      <span v-if="index" aria-hidden="true">, </span>
      <button
        type="button"
        class="card-reference"
        :data-card-code="code"
        @pointerenter="$event.pointerType === 'mouse' && preview(code, $event.currentTarget)"
        @pointerleave="$event.pointerType === 'mouse' && overlay.hide()"
        @focus="preview(code, $event.currentTarget)"
        @blur="overlay.hide()"
        @click="preview(code, $event.currentTarget, true)"
        @keydown.esc.stop.prevent="overlay.hide()"
      >{{ cards.name(code, code) }}</button>
    </template>
    <template v-if="!codes.length">{{ $t('campaign.none') }}</template>
  </span>
</template>

<style scoped>
.card-reference { display: inline; max-width: 100%; padding: 0; border: 0; border-radius: 2px; background: none; color: inherit; font: inherit; text-align: left; overflow-wrap: anywhere; text-decoration: underline; text-decoration-style: dotted; text-decoration-color: var(--text-faint); text-underline-offset: 3px; cursor: pointer; }
.card-reference:hover, .card-reference:focus-visible { color: var(--steel-bright); text-decoration-style: solid; text-decoration-color: currentColor; }
.card-reference:focus-visible { outline: 2px solid var(--steel-bright); outline-offset: 3px; }
</style>
