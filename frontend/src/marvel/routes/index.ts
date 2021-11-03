import Decks from '@/marvel/views/Decks.vue';
import NewGame from '@/marvel/views/NewGame.vue';

export default [
  {
    path: '/decks',
    name: 'Decks',
    component: Decks,
    meta: { requiresAuth: true },
    props: true,
  },
  {
    path: '/games/new',
    name: 'NewGame',
    component: NewGame,
    meta: { requiresAuth: true },
    props: true,
  },
];
