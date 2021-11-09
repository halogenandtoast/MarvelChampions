<template>
  <div id="app">
    <Nav/>
    <router-view/>
  </div>
</template>

<script lang="ts">
import { defineComponent, onMounted } from 'vue'
import { useStore } from 'vuex'
import Nav from '@/components/Nav.vue'
export default defineComponent({
  components: { Nav },
  setup() {
    const store = useStore()
    onMounted(async () => await store.dispatch('loadUserFromStorage'))
  }
})
</script>

<style lang="scss">
@font-face {
  font-family: "Exo2";
  src: url("/fonts/eco2.ttf");
}

#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
}

#nav {
  padding: 30px;

  a {
    font-weight: bold;
    color: #2c3e50;

    &.router-link-exact-active {
      color: #42b983;
    }
  }
}
</style>
