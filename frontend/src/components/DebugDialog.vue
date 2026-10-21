<script setup lang="ts">
// Run an engine message against the game by hand, for testing: the engine's own JSON,
// as Marvel.Message encodes it.
import { ref, watch } from 'vue'

const { open } = defineProps<{ open: boolean }>()
const emit = defineEmits<{ close: []; send: [message: unknown] }>()
const dialog = ref<HTMLDialogElement | null>(null)
const text = ref('{\n  "tag": "",\n  "contents": []\n}')
const invalid = ref(false)

watch(
  () => open,
  (isOpen) => {
    const el = dialog.value
    if (!el) return
    if (isOpen && !el.open) el.showModal()
    else if (!isOpen && el.open) el.close()
  },
  { flush: 'post' },
)

const submit = () => {
  try {
    emit('send', JSON.parse(text.value))
    invalid.value = false
    emit('close')
  } catch {
    invalid.value = true
  }
}
</script>

<template>
  <dialog ref="dialog" class="debug-dialog" @cancel.prevent="emit('close')" @close="emit('close')">
    <form @submit.prevent="submit">
      <h2>{{ $t('game.debug.title') }}</h2>
      <p>{{ $t('game.debug.explain') }}</p>
      <textarea v-model="text" rows="10" spellcheck="false" />
      <p v-if="invalid" class="invalid">{{ $t('game.debug.invalid') }}</p>
      <div class="actions">
        <button type="button" class="btn ghost" @click="emit('close')">{{ $t('common.cancel') }}</button>
        <button type="submit" class="btn primary">{{ $t('game.debug.send') }}</button>
      </div>
    </form>
  </dialog>
</template>

<style scoped>
.debug-dialog { width: min(36rem, 100vw - 2rem); padding: 0; border: 1px solid var(--line-strong); border-radius: 14px; background: var(--ink-2); color: var(--text); box-shadow: 0 20px 60px var(--shadow-strong); }
.debug-dialog::backdrop { background: rgb(0 0 0 / 0.6); }
form { display: grid; gap: 10px; padding: 18px; }
h2 { margin: 0; text-transform: uppercase; }
p { margin: 0; color: var(--text-muted); font-size: 0.85rem; }
textarea { font: 0.8rem/1.4 ui-monospace, Menlo, monospace; color: var(--text); background: var(--well); border: 1px solid var(--line-strong); border-radius: 8px; padding: 10px; resize: vertical; }
.invalid { color: var(--danger); }
.actions { display: flex; justify-content: flex-end; gap: 8px; }
</style>
