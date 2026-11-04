<script setup lang="ts">
// A campaign scenario's story, the rules it brings and its Campaign Instructions, read
// before the game goes on: each section a `$story.…` key with a title, paragraphs and
// bullets. Everyone at the table reads it; the player asked goes on.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import { X } from 'lucide-vue-next'
import PrimaryButton from './PrimaryButton.vue'

const { sections, canContinue, waitingOn = null, reference = false, sourceUrl = null } = defineProps<{
  sections: string[]
  canContinue: boolean
  waitingOn?: string | null
  /** Rereading a guide between games never advances the game queue. */
  reference?: boolean
  sourceUrl?: string | null
}>()
const emit = defineEmits<{ continue: []; close: [] }>()
const { t, tm, rt, te } = useI18n()

type Section = { title?: string; text?: string[]; bullets?: string[] }
const shown = computed(() =>
  sections
    .map((s) => s.replace(/^\$/, ''))
    .filter((key) => te(`${key}.title`))
    .map((key) => {
      const s = tm(key) as Record<string, unknown>
      const lines = (k: string) => ((s[k] as unknown[] | undefined) ?? []).map((m) => rt(m as Parameters<typeof rt>[0]))
      return { key, title: t(`${key}.title`), text: lines('text'), bullets: lines('bullets') } satisfies Section & { key: string }
    }),
)
</script>

<template>
  <div class="story-backdrop" @click.self="emit('close')">
    <article class="story" role="dialog" aria-modal="true" :aria-label="shown[0]?.title">
      <button type="button" class="close" :title="t('common.close')" :aria-label="t('common.close')" @click="emit('close')"><X aria-hidden="true" /></button>
      <div class="body">
        <section v-for="(s, i) in shown" :key="s.key" :class="{ lead: i === 0 }">
          <h2>{{ s.title }}</h2>
          <p v-for="(p, n) in s.text" :key="n">{{ p }}</p>
          <ul v-if="s.bullets.length">
            <li v-for="(b, n) in s.bullets" :key="n">{{ b }}</li>
          </ul>
        </section>
      </div>
      <footer>
        <a v-if="sourceUrl" :href="sourceUrl" target="_blank" rel="noopener noreferrer">{{ t('campaign.rulebook') }}</a>
        <PrimaryButton v-if="reference" :label="t('common.close')" @click="emit('close')" />
        <PrimaryButton v-else-if="canContinue" :label="t('label.continue')" @click="emit('continue')" />
        <p v-else class="waiting">{{ waitingOn ? t('game.waitingOn', { name: waitingOn }) : t('game.prompt.waiting') }}</p>
      </footer>
    </article>
  </div>
</template>

<style scoped>
.story-backdrop {
  position: fixed;
  inset: 0;
  z-index: 50;
  display: grid;
  place-items: center;
  padding: 16px;
  background: rgb(0 0 0 / 0.6);
}
.story {
  position: relative;
  display: grid;
  grid-template-rows: minmax(0, 1fr) auto;
  width: min(44rem, 100%);
  max-height: min(48rem, 100dvh - 2rem);
  border: 1px solid var(--line-strong);
  border-radius: 12px;
  background: var(--ink-2);
  color: var(--text);
  box-shadow: 0 8px 24px var(--shadow);
}
.close { position: absolute; top: 10px; right: 10px; display: grid; place-items: center; width: 36px; height: 36px; padding: 0; border: 0; border-radius: 8px; background: transparent; color: var(--text-muted); cursor: pointer; }
.close:hover { background: var(--hover-tint); color: var(--text); }
.close svg { width: 20px; height: 20px; }
.lead h2 { padding-right: 40px; }
.body { display: grid; gap: 18px; padding: 20px; overflow-y: auto; }
section { display: grid; gap: 8px; }
h2 { margin: 0; font-size: 1.05rem; text-transform: uppercase; color: var(--title); }
.lead h2 { font-size: 1.5rem; color: var(--amber); }
.lead p { font-style: italic; }
p, li { margin: 0; font-size: 0.92rem; line-height: 1.45; }
ul { display: grid; gap: 6px; margin: 0; padding-left: 1.2rem; }
li::marker { content: '» '; color: var(--crimson-bright); }
footer { display: flex; flex-wrap: wrap; align-items: center; justify-content: flex-end; gap: 12px; padding: 12px 20px; border-top: 1px solid var(--line); }
footer a { margin-right: auto; font-size: 0.85rem; }
.waiting { color: var(--text-muted); font-style: italic; }
</style>
