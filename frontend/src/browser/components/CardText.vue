<script setup lang="ts">
// Parsed card text, recursive over the token tree, so nothing reaches v-html.
import { computed } from 'vue'
import GameIcon from './GameIcon.vue'
import { parseCardText, type TextToken } from '@browser/lib/card-text'

const { text, tokens } = defineProps<{ text?: string | null; tokens?: TextToken[] }>()
const parsed = computed(() => tokens ?? parseCardText(text))
</script>

<template>
  <span class="card-text">
    <template v-for="(token, i) in parsed" :key="i">
      <template v-if="token.kind === 'text'">{{ token.value }}</template>
      <br v-else-if="token.kind === 'break'" />
      <hr v-else-if="token.kind === 'rule'" />
      <GameIcon v-else-if="token.kind === 'icon'" :name="token.name" />
      <b v-else-if="token.kind === 'trait'" class="trait">{{ token.value }}</b>
      <b v-else-if="token.style === 'bold'"><CardText :tokens="token.children" /></b>
      <i v-else><CardText :tokens="token.children" /></i>
    </template>
  </span>
</template>

<style scoped>
.card-text { white-space: pre-wrap; text-wrap: pretty; }
.card-text :deep(b) { color: var(--title); }
.trait { font-style: italic; text-transform: uppercase; letter-spacing: 0.03em; font-size: 0.92em; }
hr { margin: 0.5em 0; border: 0; border-top: 1px solid var(--line-strong); }
</style>
