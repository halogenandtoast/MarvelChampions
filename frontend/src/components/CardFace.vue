<script setup lang="ts">
// A card's scan: our own export first, MarvelCDB's when we have none, and a titled
// placeholder when neither loads. Or a card back, face down.
import { computed, ref, watch } from 'vue'
import { BACKS, useCards, type Back } from '../cards'

const { code = null, back = null, name = '', eager = false } = defineProps<{
  code?: string | null
  back?: Back | null
  /** what to call it when the card database does not know the code */
  name?: string
  eager?: boolean
}>()

const cards = useCards()
const card = computed(() => cards.card(code))
const sources = computed(() => (back ? [BACKS[back]] : code ? cards.sources(code) : []))
const attempt = ref(0)
watch(sources, () => (attempt.value = 0))
const src = computed(() => sources.value[attempt.value] ?? null)
const title = computed(() => card.value?.name ?? name ?? code ?? '')
</script>

<template>
  <span v-if="code === 'xdrone' && !back && src" class="card-face drone">
    <img :src="src" alt="" :loading="eager ? 'eager' : 'lazy'" decoding="async" draggable="false" @error="attempt++" />
    <span class="name">{{ title }}</span>
  </span>
  <img
    v-else-if="src"
    class="card-face"
    :src="src"
    :alt="title"
    :loading="eager ? 'eager' : 'lazy'"
    decoding="async"
    draggable="false"
    @error="attempt++"
  />
  <span v-else class="card-face placeholder" :data-aspect="card?.faction ?? 'basic'">
    <span class="kind">{{ card?.typeName ?? '' }}</span>
    <span class="name">{{ title }}</span>
    <span v-if="card?.subname" class="sub">{{ card.subname }}</span>
  </span>
</template>

<style scoped>
.card-face { display: block; width: 100%; height: 100%; object-fit: cover; border-radius: inherit; }
.drone { position: relative; overflow: hidden; container-type: inline-size; }
.drone img { display: block; width: 100%; height: 100%; object-fit: cover; }
.drone::after { content: ''; position: absolute; inset: 0; background: rgb(0 0 0 / 0.55); }
.drone .name { position: absolute; inset: 0; z-index: 1; display: grid; align-items: end; justify-items: center; padding: 8%; text-align: center; color: #fff; }
.placeholder {
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  gap: 4px;
  padding: 8%;
  text-align: center;
  background:
    linear-gradient(160deg, color-mix(in srgb, var(--aspect, var(--basic)) 30%, transparent), transparent 60%),
    var(--ink-3);
  border: 1px solid color-mix(in srgb, var(--aspect, var(--basic)) 50%, transparent);
  container-type: inline-size;
}
.kind { font-size: max(8px, 9cqi); text-transform: uppercase; letter-spacing: 0.08em; color: var(--text-faint); }
.name { font-family: var(--font-display); font-weight: 700; font-size: max(10px, 14cqi); line-height: 1.05; color: var(--text); }
.sub { font-size: max(8px, 9cqi); color: var(--text-muted); }
</style>
