<script setup lang="ts">
import { watchEffect } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRoute } from 'vue-router'
import NavBar from './components/NavBar.vue'

const route = useRoute()
const { t } = useI18n()

watchEffect(() => {
  document.title = t((route.meta.title as string | undefined) ?? 'appName')
})
</script>

<template>
  <NavBar :class="{ 'game-navigation': route.path.startsWith('/games/') }" />
  <RouterView :key="$route.path" />
  <!-- whose the game and its content are, on every page -->
  <i18n-t keypath="footer.notice" tag="footer" scope="global">
    <template #game><a href="https://www.fantasyflightgames.com/en/products/marvel-champions-the-card-game/" rel="noreferrer" target="_blank" tabindex="-1">{{ $t('about.gameName') }}</a></template>
    <template #ffg><a href="https://www.fantasyflightgames.com" rel="noreferrer" target="_blank" tabindex="-1">Fantasy Flight Games</a></template>
    <template #marvel><a href="https://www.marvel.com" rel="noreferrer" target="_blank" tabindex="-1">Marvel</a></template>
  </i18n-t>
</template>

<style scoped>
footer {
  flex-shrink: 0;
  margin-top: auto;
  width: 100%;
  padding: 0 8px;
  font-size: 0.6rem;
  line-height: 1.2;
  color: var(--text-faint);
  background: transparent;
  text-align: center;
  pointer-events: none;
}
footer a { color: inherit; pointer-events: auto; }
@media (width < 800px) and (orientation: portrait) {
  footer { display: none; }
}
@media (max-width: 760px) {
  :global(#nav.game-navigation) { display: none; }
}
</style>
