import { createApp } from 'vue'
import { createI18n } from 'vue-i18n'
import App from './App.vue'
import { router } from './router'
import { refreshUser } from './auth'
import { loadLocaleMessages, normalizeLocale } from './locales/messages'
import { preferredLanguage } from './locales/language'
import './style.css'

async function bootstrap() {
  const language = localStorage.getItem('language')
  const currentLanguage = language ?? preferredLanguage(navigator.language || 'en')
  const currentLocale = normalizeLocale(currentLanguage)
  if (!language) localStorage.setItem('language', currentLanguage)

  const loadedMessages: Record<string, any> = {}
  const fallback = await loadLocaleMessages('en')
  loadedMessages[fallback.locale] = fallback.messages
  if (currentLocale !== fallback.locale) {
    const current = await loadLocaleMessages(currentLocale)
    loadedMessages[current.locale] = current.messages
  }

  const i18n = createI18n({
    locale: currentLocale,
    fallbackLocale: 'en',
    legacy: false,
    warnHtmlMessage: false,
    messages: loadedMessages,
  })

  void refreshUser()
  createApp(App).use(router).use(i18n).mount('#app')
}

void bootstrap()
