<script setup lang="ts">
// Stunned, confused and tough, as the status cards are coloured: green, purple, orange.
defineProps<{ stunned?: boolean; confused?: boolean; tough?: boolean }>()
</script>

<template>
  <span v-if="stunned || confused || tough" class="statuses">
    <span v-if="stunned" class="status stunned" :title="$t('game.status.stunned')">{{ $t('game.status.stunned') }}</span>
    <span v-if="confused" class="status confused" :title="$t('game.status.confused')">{{ $t('game.status.confused') }}</span>
    <span v-if="tough" class="status tough" :title="$t('game.status.tough')">{{ $t('game.status.tough') }}</span>
  </span>
</template>

<style scoped>
.statuses { display: flex; flex-direction: column; align-items: flex-start; gap: 3px; z-index: 1; }
.status {
  padding: 2px 6px;
  border-radius: 4px;
  color: #fff;
  font-family: var(--font-display);
  font-size: 0.68rem;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  line-height: 1.2;
  box-shadow: 0 2px 5px rgb(0 0 0 / 0.55);
  border: 1px solid rgb(255 255 255 / 0.35);
}
.stunned { background: #2f8f4e; }
.confused { background: #7a4fb0; }
.tough { background: #c7741f; }
</style>
