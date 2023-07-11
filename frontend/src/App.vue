<template>
  <div class="app">
    <NavBar class="nav" />
    <router-view class="main-view" />
  </div>
</template>

<script lang="ts">
import { defineComponent, onMounted } from 'vue'
import { useStore } from 'vuex'
import NavBar from '@/components/NavBar.vue'
export default defineComponent({
  components: { NavBar },
  setup() {
    const store = useStore()
    onMounted(async () => await store.dispatch('loadUserFromStorage'))
  }
})
</script>

<style lang="scss">
@font-face {
  font-family: "Exo2";
  src: url("../public/fonts/exo2.ttf");
}

#app {
  height: 100%;
}

.app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e50;
  height: 100%;
  display: flex;
  flex-flow: column;
}

.nav {
  a {
    font-weight: bold;
    color: #2c3e50;

    &.router-link-exact-active {
      color: #42b983;
    }
  }

  flex: 0 1 auto;
}

.main-view {
  flex: 1 1 auto;
}

body {
  padding: 0;
  margin: 0;
  height: 100%;
}

html {
  height: 100%;
}
</style>
