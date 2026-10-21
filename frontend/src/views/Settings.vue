<script setup lang="ts">
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { currentUser, updateSettings } from '../auth'
import { loadLocaleMessages, normalizeLocale, supportedLocales } from '../locales/messages'

// The language is kept in this browser, its messages loaded the first time it is chosen.
const { locale, availableLocales, setLocaleMessage } = useI18n({ useScope: 'global' })
const language = ref(normalizeLocale(locale.value))
const languageName = (code: string) => new Intl.DisplayNames([code], { type: 'language' }).of(code) ?? code

const updateLanguage = async () => {
  const chosen = normalizeLocale(language.value)
  if (!availableLocales.includes(chosen)) {
    const loaded = await loadLocaleMessages(chosen)
    setLocaleMessage(loaded.locale, loaded.messages)
  }
  locale.value = chosen
  localStorage.setItem('language', chosen)
  document.documentElement.lang = chosen
}

const spectators = ref(currentUser.value?.allowSpectatorsByDefault === true)
const saved = ref(false)
const saveSpectators = async () => {
  saved.value = false
  await updateSettings({ allowSpectatorsByDefault: spectators.value })
  saved.value = true
}
</script>

<template>
  <div class="page-container">
    <div class="settings">
      <h1 class="page-title">{{ $t('settings.title') }}</h1>
      <section class="box panel">
        <label>
          {{ $t('settings.language') }}
          <select v-model="language" class="input" @change="updateLanguage">
            <option v-for="code in supportedLocales" :key="code" :value="code">{{ languageName(code) }}</option>
          </select>
        </label>
        <p class="hint">{{ $t('settings.languageHint') }}</p>
      </section>
      <section v-if="currentUser" class="box panel">
        <span class="title">{{ $t('settings.spectators') }}</span>
        <label class="switch">
          <input v-model="spectators" type="checkbox" role="switch" @change="saveSpectators" />
          <span class="track" aria-hidden="true" />
          {{ $t('settings.allowByDefault') }}
          <span v-if="saved" class="saved">{{ $t('settings.saved') }}</span>
        </label>
        <p class="hint">{{ $t('settings.spectatorsHint') }}</p>
      </section>
    </div>
  </div>
</template>

<style scoped>
.settings { width: min(560px, 100%); margin: 0 auto; display: grid; gap: 14px; }
.settings .page-title { margin: 0; }
.panel { display: grid; gap: 10px; }
label:not(.switch) { display: grid; gap: 6px; font-size: 0.85rem; color: var(--text-muted); }
.title { font-size: 0.85rem; color: var(--text-muted); }
.hint { margin: 0; font-size: 0.8rem; color: var(--text-faint); }
.saved { font-size: 0.75rem; color: var(--health); }
</style>
