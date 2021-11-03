<template>
  <div class="home">
    <router-link to="/games/new" custom v-slot="{ navigate }">
      <button @click="navigate">New Game</button>
    </router-link>
  </div>
</template>
