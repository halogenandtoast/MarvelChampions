<script lang="ts" setup>
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRouter } from 'vue-router'
import { signUp } from '../auth'
import AuthForm from '../components/AuthForm.vue'

const { t } = useI18n()
const router = useRouter()
const username = ref('')
const email = ref('')
const password = ref('')
const error = ref<string | null>(null)
const submitting = ref(false)

const submit = async () => {
  submitting.value = true
  error.value = null
  try {
    await signUp(username.value, email.value, password.value)
    await router.push('/')
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('auth.couldNotCreateAccount')
  } finally {
    submitting.value = false
  }
}
</script>

<template>
  <AuthForm :title="$t('auth.createAccount')" :submit-label="$t('auth.signUp')" :error="error" :submitting="submitting" @submit="submit">
    <input v-model="username" type="text" autocomplete="username" :placeholder="$t('auth.username')" required />
    <input v-model="email" type="email" autocomplete="email" :placeholder="$t('auth.email')" required />
    <input v-model="password" type="password" autocomplete="new-password" minlength="8" :placeholder="$t('auth.newPassword')" required />
    <template #links>
      {{ $t('auth.haveAccount') }} <RouterLink to="/sign-in">{{ $t('auth.signIn') }}</RouterLink>
    </template>
  </AuthForm>
</template>
