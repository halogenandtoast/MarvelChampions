<script setup lang="ts">
/**
 * A single card at its own URL, for linking and sharing.
 */
import '@browser/browser.css'
import { computed, onMounted } from 'vue'
import { useRoute } from 'vue-router'
import CardDetail from '@browser/components/CardDetail.vue'
import { useCardStore } from '@browser/stores/cards'

const route = useRoute()
const store = useCardStore()

const code = computed(() => String(route.params.code ?? ''))
const card = computed(() => store.cardByCode(code.value))

onMounted(() => store.load())
</script>

<template>
  <div class="page-container">
    <main class="page-content card-browser detail-page">
      <RouterLink class="button button--ghost back" to="/cards">&#8592; {{ $t('browser.card.back') }}</RouterLink>

      <p v-if="store.state === 'loading'" class="notice">{{ $t('browser.common.loading') }}</p>
      <i18n-t v-else-if="!card" keypath="browser.card.notFound" scope="global" tag="p" class="notice">
        <template #code><code>{{ code }}</code></template>
      </i18n-t>
      <CardDetail v-else :card="card" />
    </main>
  </div>
</template>

<style scoped>
.detail-page { max-width: 64rem; }

.back {
  margin-bottom: var(--space-4);
}

.notice {
  padding: var(--space-6);
  color: var(--c-text-muted);
  text-align: center;
}

code {
  font-family: var(--font-mono);
}
</style>
