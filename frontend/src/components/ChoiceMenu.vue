<script setup lang="ts">
// What can be done with a card that offers more than one choice: playing it or using its
// ability, attacking or thwarting with an ally.
import { onMounted, ref, useTemplateRef } from 'vue'
import { onClickOutside, onKeyStroke } from '@vueuse/core'
import { useChoiceLabel } from '../composables/useChoices'
import type { ChoiceView } from '../server'

const { options, anchor, cardName = '' } = defineProps<{ options: ChoiceView[]; anchor: HTMLElement; cardName?: string }>()
const emit = defineEmits<{ choose: [index: number]; close: [] }>()

const label = useChoiceLabel()
const menu = useTemplateRef<HTMLElement>('menu')
const GAP = 8
const PAD = 8
const place = ref({ top: 0, left: 0 })

// beside the card, on its right unless that runs off the screen
onMounted(() => {
  const rect = anchor.getBoundingClientRect()
  const box = menu.value?.getBoundingClientRect()
  const width = box?.width ?? 200
  const height = box?.height ?? 0
  const left = rect.right + GAP + width + PAD > window.innerWidth ? rect.left - GAP - width : rect.right + GAP
  place.value = {
    top: Math.max(PAD, Math.min(rect.top, window.innerHeight - height - PAD)),
    left: Math.max(PAD, left),
  }
  menu.value?.querySelector('button')?.focus()
})

onClickOutside(menu, () => emit('close'), { ignore: [anchor] })
onKeyStroke('Escape', () => emit('close'))
</script>

<template>
  <Teleport to="body">
    <div ref="menu" class="choice-menu" role="menu" :style="{ top: `${place.top}px`, left: `${place.left}px` }">
      <p v-if="cardName" class="title">{{ cardName }}</p>
      <button v-for="option in options" :key="option.index" type="button" role="menuitem" @click="emit('choose', option.index)">
        {{ label(option, cardName) }}
      </button>
    </div>
  </Teleport>
</template>

<style scoped>
.choice-menu {
  position: fixed;
  z-index: 9000;
  display: flex;
  flex-direction: column;
  gap: 4px;
  min-width: 11rem;
  max-width: 20rem;
  padding: 6px;
  background: var(--ink-3);
  border: 1px solid var(--line-strong);
  border-radius: 10px;
  box-shadow: 0 12px 32px var(--shadow-strong);
}
.title {
  margin: 2px 6px 4px;
  font-family: var(--font-display);
  font-size: 0.95rem;
  font-weight: 700;
  color: var(--text-muted);
  text-transform: uppercase;
  letter-spacing: 0.04em;
}
.choice-menu button {
  padding: 8px 12px;
  font: inherit;
  font-size: 0.85rem;
  font-weight: 600;
  text-align: left;
  color: var(--text);
  background: var(--ink-4);
  border: 1px solid var(--line);
  border-radius: 7px;
  cursor: pointer;
}
.choice-menu button:hover,
.choice-menu button:focus-visible {
  background: color-mix(in srgb, var(--interactive) 22%, var(--ink-4));
  border-color: var(--interactive);
  outline: none;
}
</style>
