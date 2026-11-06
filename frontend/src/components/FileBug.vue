<script setup lang="ts">
/**
 * Report a bug, as Arkham does: the server uploads the game's export, and a new tab
 * opens GitHub's new-issue page with the report filled in, for the reporter to submit.
 * A native `<dialog>`, as `KeyHelp` is.
 */
import { ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { fileBug } from '../api'
import PrimaryButton from './PrimaryButton.vue'

const { open, gameId, description: initialDescription = '' } = defineProps<{
  open: boolean
  gameId: string
  description?: string
}>()
const emit = defineEmits<{ close: [] }>()
const { t } = useI18n()

const dialog = ref<HTMLDialogElement | null>(null)
const title = ref('')
const description = ref('')
const submitting = ref(false)
const failed = ref(false)

watch(
  () => open,
  (isOpen) => {
    const el = dialog.value
    if (!el) return
    if (isOpen && !el.open) {
      description.value = initialDescription
      failed.value = false
      el.showModal()
    } else if (!isOpen && el.open) el.close()
  },
  { flush: 'post' },
)

const submit = async () => {
  // open the tab while the click still counts, so a popup blocker lets it through
  const tab = window.open('', '_blank')
  submitting.value = true
  failed.value = false
  try {
    const { url } = await fileBug(gameId)
    const body = `${description.value}\n\ngame: ${window.location.href}\nfile: ${url}`
    const params = new URLSearchParams({
      labels: 'bug',
      title: title.value,
      body,
      assignee: 'halogenandtoast',
      projects: 'halogenandtoast/8',
    })
    const issue = `https://github.com/halogenandtoast/MarvelChampions/issues/new?${params}`
    if (tab) tab.location.href = issue
    else window.open(issue, '_blank')
    title.value = ''
    description.value = ''
    emit('close')
  } catch {
    tab?.close()
    failed.value = true
  } finally {
    submitting.value = false
  }
}
</script>

<template>
  <dialog ref="dialog" class="file-bug" :aria-label="t('game.fileBug.title')" @cancel.prevent="emit('close')" @close="emit('close')">
    <form @submit.prevent="submit">
      <h2>{{ t('game.fileBug.title') }}</h2>
      <p>{{ t('game.fileBug.explain') }}</p>
      <input v-model="title" type="text" required :placeholder="t('game.fileBug.titlePlaceholder')" />
      <textarea v-model="description" required rows="6" :placeholder="t('game.fileBug.descriptionPlaceholder')"></textarea>
      <p v-if="failed" class="failed">{{ t('game.fileBug.failed') }}</p>
      <div class="actions">
        <button type="button" class="cancel" @click="emit('close')">{{ t('common.cancel') }}</button>
        <PrimaryButton type="submit" :label="submitting ? t('game.fileBug.uploading') : t('game.fileBug.submit')" :disabled="submitting" />
      </div>
    </form>
  </dialog>
</template>

<style scoped>
.file-bug {
  width: min(32rem, 100vw - 2rem);
  padding: 0;
  border: 1px solid var(--line-strong);
  border-radius: 5px;
  background: var(--panel);
  color: var(--text);
  box-shadow: 0 8px 24px var(--shadow);
}
.file-bug::backdrop { background: rgb(0 0 0 / 0.6); }

form { display: flex; flex-direction: column; gap: 10px; padding: 16px; }
h2 { margin: 0; color: var(--title); font-size: 1.1rem; }
p { margin: 0; color: var(--text-muted); font-size: 0.875rem; }

input, textarea {
  font: inherit;
  color: inherit;
  background: var(--input-background);
  border: 1px solid var(--line);
  border-radius: 3px;
  padding: 8px;
}
textarea { resize: vertical; }

.failed { color: var(--danger); }
.actions { display: flex; justify-content: flex-end; gap: 10px; }
.cancel { background: none; border: 0; color: inherit; font: inherit; cursor: pointer; }
</style>
