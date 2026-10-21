<script setup lang="ts">
import { useRouter } from 'vue-router'
import { currentUser, signOut } from '../auth'
import GameMenu from './GameMenu.vue'

const router = useRouter()

const leave = async () => {
  signOut()
  await router.push('/sign-in')
}
</script>

<template>
  <header id="nav">
    <nav class="main-links">
      <RouterLink to="/" class="home-link">
        <img src="/icon.svg" alt="" class="logo" />
        <span class="wordmark">Marvel <b>Champions</b></span>
      </RouterLink>
      <RouterLink v-if="currentUser" to="/" class="nav-link nav-home" exact-active-class="current">{{ $t('nav.home') }}</RouterLink>
      <RouterLink to="/cards" class="nav-link" active-class="current">{{ $t('nav.cards') }}</RouterLink>
      <RouterLink v-if="currentUser" to="/decks" class="nav-link" active-class="current">{{ $t('nav.decks') }}</RouterLink>
      <RouterLink to="/watch" class="nav-link" active-class="current">{{ $t('nav.watch') }}</RouterLink>
      <RouterLink to="/about" class="nav-link" active-class="current">{{ $t('nav.about') }}</RouterLink>
      <RouterLink v-if="currentUser?.admin" to="/admin" class="nav-link" active-class="current">{{ $t('nav.admin') }}</RouterLink>
    </nav>
    <div class="user-links">
      <GameMenu v-if="currentUser" class="user-menu" align="right">
        <span class="avatar">{{ currentUser.username.slice(0, 1).toUpperCase() }}</span>
        {{ currentUser.username }}
        <template #items>
          <RouterLink to="/settings">{{ $t('settings.title') }}</RouterLink>
          <button type="button" @click="leave">{{ $t('auth.signOut') }}</button>
        </template>
      </GameMenu>
      <template v-else>
        <RouterLink to="/sign-in">{{ $t('auth.signIn') }}</RouterLink>
        <RouterLink to="/sign-up" class="btn primary sign-up">{{ $t('auth.signUp') }}</RouterLink>
      </template>
    </div>
  </header>
</template>

<style scoped>
#nav {
  display: flex;
  align-items: stretch;
  height: var(--nav-height);
  flex-shrink: 0;
  position: relative;
  z-index: 100;
  background: var(--nav-background);
  border-bottom: 1px solid var(--line);
  backdrop-filter: blur(8px);
}
/* a thin red rule along the top, the one flourish */
#nav::before { content: ''; position: absolute; inset: 0 0 auto 0; height: 2px; background: linear-gradient(90deg, var(--crimson), transparent 70%); }
.main-links { display: flex; align-items: stretch; flex: 1; min-width: 0; }
.home-link, .nav-link {
  display: flex;
  align-items: center;
  padding: 0 14px;
  color: var(--nav-link);
  font-size: 0.875rem;
  font-weight: 500;
  text-decoration: none;
  position: relative;
  white-space: nowrap;
  transition: color 0.15s;
}
.home-link { gap: 10px; padding-right: 22px; }
.logo { width: 26px; height: 26px; }
.wordmark { font-family: var(--font-display); font-size: 1.25rem; font-weight: 600; letter-spacing: 0.02em; text-transform: uppercase; color: var(--text-muted); }
.wordmark b { font-weight: 800; color: var(--title); }
.nav-link:hover { color: var(--text); }
.nav-link.current { color: var(--title); }
.nav-link.current::after {
  content: '';
  position: absolute;
  bottom: 0;
  left: 12px;
  right: 12px;
  height: 2px;
  background: var(--crimson);
  border-radius: 2px 2px 0 0;
}
.user-links { display: flex; align-items: center; gap: 14px; padding: 0 14px; font-size: 0.875rem; }
.user-links a { color: var(--nav-link); text-decoration: none; font-weight: 500; }
.user-links a:hover { color: var(--text); }
.user-links .sign-up { color: #fff; padding: 6px 12px; }
.avatar { display: grid; place-items: center; width: 24px; height: 24px; border-radius: 50%; background: var(--steel-deep); color: var(--steel-bright); font-size: 0.75rem; font-weight: 800; }
.user-menu :deep(.menu-items) { border-radius: 0 0 10px 10px; overflow: hidden; }
.user-menu :deep(.menu-items button),
.user-menu :deep(.menu-items a) {
  background: none;
  border: 0;
  color: var(--text);
  font: inherit;
  padding: 10px 14px;
  text-align: left;
  text-decoration: none;
  cursor: pointer;
}
.user-menu :deep(.menu-items button:hover),
.user-menu :deep(.menu-items a:hover) { background: var(--hover-tint); }
@media (max-width: 768px) {
  .main-links { overflow-x: auto; scrollbar-width: none; }
  .wordmark, .nav-home { display: none; }
  .home-link, .nav-link { padding: 0 9px; }
  .user-links { padding: 0 8px; }
}
</style>
