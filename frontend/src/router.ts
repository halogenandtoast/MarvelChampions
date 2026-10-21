import { createRouter, createWebHashHistory } from 'vue-router'
import { currentUser } from './auth'
import Home from './views/Home.vue'

export const router = createRouter({
  history: createWebHashHistory(),
  routes: [
    { path: '/', component: Home, meta: { title: 'appName', requiresAuth: true } },
    { path: '/new-game', component: () => import('./views/NewGame.vue'), meta: { title: 'newGame.title', requiresAuth: true } },
    { path: '/games/:id/spectate', component: () => import('./views/Game.vue'), meta: { title: 'common.game', spectate: true } },
    { path: '/watch', component: () => import('./views/Watch.vue'), meta: { title: 'nav.watch' } },
    { path: '/admin', component: () => import('./views/Admin.vue'), meta: { title: 'nav.admin', requiresAuth: true, requiresAdmin: true } },
    { path: '/games/:id', component: () => import('./views/Game.vue'), meta: { title: 'common.game', requiresAuth: true } },
    { path: '/cards', component: () => import('./browser/views/BrowseView.vue'), meta: { title: 'nav.cards' } },
    { path: '/cards/:code', component: () => import('./browser/views/CardDetailView.vue'), meta: { title: 'titles.card' } },
    { path: '/decks', name: 'decks', component: () => import('./browser/views/DecksView.vue'), meta: { title: 'nav.decks', requiresAuth: true } },
    { path: '/decks/:id', name: 'deck', component: () => import('./browser/views/DeckDetailView.vue'), meta: { title: 'titles.deck', requiresAuth: true } },
    { path: '/about', component: () => import('./views/About.vue'), meta: { title: 'nav.about' } },
    { path: '/settings', component: () => import('./views/Settings.vue'), meta: { title: 'settings.title' } },
    { path: '/sign-in', component: () => import('./views/SignIn.vue'), meta: { title: 'titles.signIn', guest: true } },
    { path: '/sign-up', component: () => import('./views/SignUp.vue'), meta: { title: 'titles.signUp', guest: true } },
  ],
})

router.beforeEach((to) => {
  if (to.meta.requiresAuth && !currentUser.value) return { path: '/sign-in', query: { next: to.fullPath } }
  if (to.meta.requiresAdmin && !currentUser.value?.admin) return { path: '/' }
  if (to.meta.guest && currentUser.value) return { path: '/' }
})

// After a deploy, an open page still asks for the old build's chunks, which are gone.
// Load the page afresh at the link followed instead, once: a second failure soon after
// is a real one, left to show.
const STALE_KEY = 'reloaded-for-stale-build'
router.onError((error, to) => {
  if (!/dynamically imported module|Importing a module script failed|error loading dynamically imported module/i.test(String(error?.message))) return
  const last = Number(sessionStorage.getItem(STALE_KEY) ?? 0)
  if (Date.now() - last < 10_000) return
  sessionStorage.setItem(STALE_KEY, String(Date.now()))
  window.location.replace(router.resolve(to).href)
  window.location.reload()
})
