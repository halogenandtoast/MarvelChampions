<script lang="ts" setup>
withDefaults(defineProps<{ label: string; danger?: boolean; type?: 'button' | 'submit' }>(), { danger: false, type: 'button' })
defineEmits(['click'])
</script>

<template>
  <button class="primary-btn" :class="{ danger }" :type="type" @click="$emit('click')">{{ label }}</button>
</template>

<style scoped>
.primary-btn {
  height: fit-content;
  align-self: center;
  background-color: var(--button-primary);
  border-radius: 3px;
  outline: 0;
  padding: 10px 15px;
  text-transform: uppercase;
  color: var(--button-primary-text);
  border: 0;
  font: inherit;
  font-weight: bolder;
  cursor: pointer;
  transition: background-color 0.3s linear;
}
.primary-btn:hover { background: var(--button-primary-hover); }
.primary-btn.danger { background-color: var(--danger); color: var(--text); }
.primary-btn:disabled { background: var(--button-disabled); color: var(--button-disabled-text); cursor: not-allowed; }
@media (max-width: 768px) {
  .primary-btn { padding: 6px 10px; font-size: 0.82em; }
}
</style>
