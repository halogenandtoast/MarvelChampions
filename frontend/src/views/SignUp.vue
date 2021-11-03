<template>
  <form @submit.prevent="register">
    <header><i class="secret"></i></header>
    <section>
      <div>
        <input
          v-model="registration.username"
          type="text"
          placeholder="Username"
        />
      </div>
      <div>
        <input
          v-model="registration.email"
          type="email"
          placeholder="Email"
        />
      </div>
      <div>
        <input
          v-model="registration.password"
          type="password"
          placeholder="Password"
        />
      </div>
      <div>
        <button>Register</button>
      </div>
    </section>
  </form>
</template>

<script lang="ts">
import { defineComponent, reactive } from 'vue'
import { useStore } from 'vuex'
import { useRoute, useRouter } from 'vue-router'
import { Registration } from '@/types'
export default defineComponent({
  setup() {
    const route = useRoute()
    const router = useRouter()
    const store = useStore()
    const registration = reactive<Registration>({
      username: '',
      email: '',
      password: '',
    })
    async function register() {
      await store.dispatch('register', registration)
      const { nextUrl } = route.query
      if (nextUrl) {
        router.push({ path: nextUrl as string })
      } else {
        router.push({ path: '/' })
      }
    }
    return { registration, register }
  }
});
</script>
