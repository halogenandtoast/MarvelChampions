<script setup lang="ts">
import { useTemplateRef } from 'vue'

// Asks before deleting something: a native dialog, so Escape closes it, and Delete has
// the focus, so Enter confirms. What is to be deleted goes in the slot.
defineProps<{ title: string }>()
const emit = defineEmits<{ confirm: [] }>()

const dialog = useTemplateRef<HTMLDialogElement>('dialog')
const close = () => dialog.value?.close()
// a click on the dialog itself landed on its backdrop
const onBackdrop = (event: MouseEvent) => {
  if (event.target === dialog.value) close()
}
defineExpose({ open: () => dialog.value?.showModal() })
</script>

<template>
  <dialog ref="dialog" class="confirm-delete" :aria-label="title" @click="onBackdrop">
    <h2>{{ title }}</h2>
    <div class="subject"><slot /></div>
    <div class="actions">
      <button type="button" @click="close">{{ $t('common.cancel') }}</button>
      <button type="button" class="confirm" autofocus @click="(close(), emit('confirm'))">{{ $t('common.delete') }}</button>
    </div>
  </dialog>
</template>

<style scoped>
.confirm-delete {
  width: min(26rem, 100vw - 2rem);
  padding: 18px 20px;
  border: 1px solid var(--line-strong);
  border-radius: 8px;
  background: var(--panel);
  color: var(--text);
  box-shadow: 0 10px 40px var(--shadow-strong);
}
.confirm-delete::backdrop { background: rgb(0 0 0 / 0.6); backdrop-filter: blur(2px); }
.confirm-delete h2 { margin: 0 0 12px; font-size: 1.1em; color: var(--title); }
.subject { display: flex; align-items: center; gap: 12px; }
.actions { display: flex; justify-content: flex-end; gap: 8px; margin-top: 18px; }
.actions button {
  background: transparent;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  color: var(--text);
  cursor: pointer;
  font: inherit;
  font-size: 0.85em;
  font-weight: 700;
  padding: 6px 12px;
  text-transform: uppercase;
}
.actions button.confirm { background: var(--danger); border-color: var(--danger); }
.actions button:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; }
</style>
