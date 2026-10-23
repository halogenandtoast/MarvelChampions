<script lang="ts" setup>
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRoute, useRouter } from 'vue-router'
import { playAsGuest, signIn } from '../auth'
import AuthForm from '../components/AuthForm.vue'

const { t } = useI18n()
const route = useRoute()
const router = useRouter()
const login = ref('')
const password = ref('')
const error = ref<string | null>(null)
const submitting = ref(false)

const submit = (guest = false) => async () => {
  submitting.value = true
  error.value = null
  try {
    await (guest ? playAsGuest() : signIn(login.value, password.value))
    await router.push(typeof route.query.next === 'string' ? route.query.next : '/')
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('auth.couldNotSignIn')
  } finally {
    submitting.value = false
  }
}
</script>

<template>
  <AuthForm :title="$t('auth.signIn')" :submit-label="$t('auth.signIn')" :error="error" :submitting="submitting" @submit="submit()()">
    <input v-model="login" type="text" autocomplete="username" :placeholder="$t('auth.usernameOrEmail')" required />
    <input v-model="password" type="password" autocomplete="current-password" :placeholder="$t('auth.password')" required />
    <template #links>
      {{ $t('auth.newHere') }} <RouterLink to="/sign-up">{{ $t('auth.createAccount') }}</RouterLink>
      {{ $t('auth.or') }} <a href="#" @click.prevent="submit(true)()">{{ $t('auth.playAsGuest') }}</a>
    </template>
  </AuthForm>
</template>
