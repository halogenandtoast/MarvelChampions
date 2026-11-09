<script setup lang="ts">
import { ref, watch } from 'vue'
import type { DebugCard } from '../composables/useCardDebug'
const props = defineProps<{ card: DebugCard | null; save: (card: DebugCard, values: Record<string, number | boolean>) => Promise<boolean> }>()
const emit = defineEmits<{ close: [] }>()
const dialog = ref<HTMLDialogElement | null>(null)
const values = ref<Record<string, number | boolean>>({})
const busy = ref(false)
const failed = ref(false)
watch(() => props.card, card => {
  values.value = { ...card?.values }
  failed.value = false
  if (card) dialog.value?.showModal()
  else dialog.value?.close()
}, { flush: 'post' })
const submit = async () => {
  if (!props.card || busy.value) return
  busy.value = true
  try {
    if (await props.save(props.card, values.value)) emit('close')
    else failed.value = true
  } catch { failed.value = true }
  finally { busy.value = false }
}
</script>
<template>
  <dialog ref="dialog" class="card-debug" @cancel.prevent="!busy && emit('close')" @close="emit('close')">
    <form @submit.prevent="submit">
      <h2>{{ card?.name }}</h2>
      <p>{{ $t('game.debug.cardExplain') }}</p>
      <div class="fields">
        <label v-for="(value, key) in values" :key="key">
          <span>{{ $t(`game.debug.fields.${key}`) }}</span>
          <input v-if="typeof value === 'boolean'" v-model="values[key]" type="checkbox" :disabled="busy" />
          <input v-else v-model.number="values[key]" type="number" min="0" :max="key === 'stunned' || key === 'confused' ? 2 : key === 'tough' && card?.kind === 'Villain' ? 1 : undefined" step="1" required :disabled="busy" />
        </label>
      </div>
      <p v-if="!Object.keys(values).length">{{ $t('game.debug.notInPlay') }}</p>
      <p v-if="failed" role="alert">{{ $t('game.debug.failed') }}</p>
      <footer>
        <button type="button" class="btn ghost" :disabled="busy" @click="emit('close')">{{ $t('common.cancel') }}</button>
        <button v-if="Object.keys(values).length" type="submit" class="btn primary" :disabled="busy">{{ $t(busy ? 'game.debug.saving' : 'game.debug.apply') }}</button>
      </footer>
    </form>
  </dialog>
</template>
<style scoped>
.card-debug { width: min(30rem, calc(100vw - 1rem)); max-height: calc(100dvh - 2rem); padding: 0; border: 1px solid var(--line-strong); border-radius: 14px; background: var(--ink-2); color: var(--text); }
.card-debug::backdrop { background: rgb(0 0 0 / .65); }
form { padding: 20px; display: grid; gap: 16px; }
h2, p { margin: 0; }
p { color: var(--text-muted); font-size: .9rem; }
.fields { display: grid; gap: 8px; }
label { display: flex; align-items: center; justify-content: space-between; gap: 16px; min-height: 44px; }
input[type=number] { width: 6rem; min-height: 44px; padding: 8px; font: inherit; background: var(--well); color: var(--text); border: 1px solid var(--line-strong); border-radius: 6px; }
input[type=checkbox] { width: 24px; height: 24px; }
footer { display: flex; justify-content: flex-end; gap: 8px; }
footer button { min-height: 44px; }
</style>
