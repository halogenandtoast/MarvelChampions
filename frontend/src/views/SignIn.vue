<template>
  <form @submit.prevent="authenticate">
    <header><i class="secret"></i></header>
    <section>
      <div>
        <input
          v-model="credentials.email"
          type="email"
          placeholder="Email"
        />
      </div>
      <div>
        <input
          v-model="credentials.password"
          type="password"
          placeholder="Password"
        />
      </div>
      <div>
        <button>Log in</button>
      </div>
    </section>
  </form>
</template>

<script lang="ts">
import { useStore } from 'vuex'
import { useRoute, useRouter } from 'vue-router'
import { defineComponent, reactive } from 'vue'
import { Credentials } from '../types'
export default defineComponent({
  setup() {
    const store = useStore()
    const route = useRoute()
    const router = useRouter()
    const credentials = reactive<Credentials>({
      email: '',
      password: '',
    })
    async function authenticate() {
      await store.dispatch('authenticate', credentials)
      const { nextUrl } = route.query
      if (nextUrl) {
        router.push({ path: nextUrl as string })
      } else {
        router.push({ path: '/' })
      }
    }
    return { credentials, authenticate }
  }
})
</script>
