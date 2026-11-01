<script setup lang="ts">
// Stunned, confused and tough: their status cards, tucked under the corner of the card.
defineProps<{ stunCards?: number; confuseCards?: number; stunned?: boolean; confused?: boolean; tough?: boolean; toughCards?: number }>()
</script>

<template>
  <span v-if="stunned || confused || tough" class="statuses">
    <img v-for="n in (stunCards ?? (stunned ? 1 : 0))" :key="`stun-${n}`" src="/images/tokens/stunned.avif" :alt="$t('game.status.stunned')" draggable="false" />
    <img v-for="n in (confuseCards ?? (confused ? 1 : 0))" :key="`confuse-${n}`" src="/images/tokens/confused.avif" :alt="$t('game.status.confused')" draggable="false" />
    <img v-for="n in (toughCards ?? (tough ? 1 : 0))" :key="`tough-${n}`" src="/images/tokens/tough.avif" :alt="$t('game.status.tough')" draggable="false" />
  </span>
</template>

<style scoped>
.statuses { display: flex; flex-direction: column; align-items: flex-start; z-index: 1; }
img { width: max(40px, 52cqi); height: auto; border-radius: 4px; box-shadow: 0 2px 5px rgb(0 0 0 / 0.55); }
img + img { margin-top: max(-18px, -22cqi); }
</style>
