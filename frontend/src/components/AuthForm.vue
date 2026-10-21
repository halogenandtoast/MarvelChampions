<script lang="ts" setup>
import { useServerText } from '../i18n'

const tr = useServerText()
defineProps<{ title: string; error: string | null; submitting: boolean; submitLabel: string }>()
defineEmits<{ submit: [] }>()
</script>

<template>
  <div class="page-container">
    <form class="auth-form" @submit.prevent="$emit('submit')">
      <header>
        <img src="/icon.svg" alt="" class="logo" />
        <h2>{{ title }}</h2>
        <p class="tagline">{{ $t('auth.tagline') }}</p>
      </header>
      <div v-if="error" class="error" role="alert">{{ tr(error) }}</div>
      <section>
        <slot></slot>
        <button type="submit" :disabled="submitting">{{ submitting ? $t('auth.oneMoment') : submitLabel }}</button>
      </section>
      <section class="links">
        <slot name="links"></slot>
      </section>
    </form>
  </div>
</template>

<style scoped>
.auth-form { margin: 9vh auto 0; width: min(400px, 92vw); }
header { text-align: center; margin-bottom: 18px; }
.logo { display: block; width: 64px; height: 64px; margin: 0 auto; filter: drop-shadow(0 8px 24px rgb(184 50 58 / 0.35)); }
h2 { margin: 12px 0 0; font-size: 2.2rem; line-height: 1; text-transform: uppercase; color: var(--title); }
.tagline { margin: 6px 0 0; color: var(--text-muted); font-size: 0.9rem; }
section { display: grid; gap: 10px; padding: 18px; border-radius: 14px; background: var(--ink-2); border: 1px solid var(--line); }
section.links { background: none; border: 0; text-align: center; color: var(--text-muted); font-size: 0.9em; }
section :deep(input) { width: 100%; padding: 13px 14px; font: inherit; color: var(--text); background: var(--well); border: 1px solid var(--line-strong); border-radius: 9px; outline: 0; }
section :deep(input:focus) { border-color: var(--steel); box-shadow: 0 0 0 3px rgb(91 141 184 / 0.2); }
section :deep(input::placeholder) { color: var(--text-faint); }
button { padding: 13px; border: 0; border-radius: 9px; background: var(--crimson); color: #fff; font: inherit; font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase; cursor: pointer; }
button:hover { background: var(--crimson-bright); }
button:disabled { background: var(--ink-4); color: var(--text-faint); cursor: wait; }
.error { margin: 0 0 12px; padding: 10px 12px; border-radius: 9px; border: 1px solid var(--danger); background: rgb(214 80 88 / 0.1); color: #f0a4a9; }
</style>
