<script setup lang="ts">
// Everything known about one card: the scan, its statistics, its rules text, and its
// other face, for an identity's alter-ego or a scheme's B side.
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import CardImage from './CardImage.vue'
import FoldingIdentity from '../../components/FoldingIdentity.vue'
import CardStats from './CardStats.vue'
import CardText from './CardText.vue'
import { useCardStore } from '@browser/stores/cards'
import type { Card } from '@browser/types/card'

const { card } = defineProps<{ card: Card }>()
const { t } = useI18n()
const store = useCardStore()

/** Which face is showing: the card, the card it is linked to, then a fold-out third. The dialog drives it with the arrows. */
const imageIndex = defineModel<number>('imageIndex', { default: 0 })
const other = computed(() => store.cardByCode(card.linkedTo))
const third = computed(() => (other.value ? store.thirdFace(card) : undefined))
const faces = computed(() => [card, other.value, third.value].filter((c): c is Card => !!c))
const shown = computed(() => faces.value[imageIndex.value] ?? card)
// two hero forms share a name (Ant-Man): told apart by the trait naming the form, printed last
const faceName = (face: Card) =>
  faces.value.filter((f) => f.name === face.name).length > 1 && face.traits?.length ? `${face.name} (${face.traits.at(-1)})` : face.name

const facts = computed(() => {
  const c = shown.value
  const rows: { label: string; value: string }[] = []
  rows.push({ label: t('browser.card.facts.pack'), value: `${c.packName}${c.position ? ` #${c.position}` : ''}` })
  if (c.setName) rows.push({ label: t('browser.card.facts.set'), value: c.setName })
  if (c.quantity) rows.push({ label: t('browser.card.facts.quantity'), value: String(c.quantity) })
  if (c.deckLimit) rows.push({ label: t('browser.card.facts.deckLimit'), value: String(c.deckLimit) })
  if (c.illustrator) rows.push({ label: t('browser.card.facts.illustrator'), value: c.illustrator })
  rows.push({ label: t('browser.card.facts.code'), value: c.code })
  return rows
})
const marvelcdb = computed(() => `https://marvelcdb.com/card/${shown.value.code}`)
</script>

<template>
  <div class="detail-container">
    <article class="detail" :data-aspect="shown.faction">
      <div class="art">
        <div v-if="third" class="folding-frame"><FoldingIdentity :faces="faces.map((c) => c.code)" :code="shown.code" /></div>
        <!-- a two-sided card turns over to its other face -->
        <div v-else-if="other" class="flip" :class="{ 'is-flipped': imageIndex === 1 }">
          <CardImage class="side" :card="card" eager />
          <CardImage class="side side--back" :card="other" eager />
        </div>
        <CardImage v-else :card="shown" eager />
        <div v-if="other" class="faces" role="tablist" :aria-label="$t('browser.card.faces')">
          <button
            v-for="(face, i) in faces"
            :key="face.code"
            type="button"
            class="button"
            role="tab"
            :aria-selected="imageIndex === i"
            :class="{ 'is-active': imageIndex === i }"
            @click="imageIndex = i"
          >
            {{ faceName(face) }}
          </button>
        </div>
      </div>

      <div class="body stack">
        <header>
          <p class="line">
            <span class="aspect-chip" :data-aspect="shown.faction">{{ shown.factionName ?? shown.faction }}</span>
            <span>{{ shown.typeName ?? shown.type }}</span>
            <span v-if="shown.stage">{{ $t('browser.card.stage', { stage: shown.stage }) }}</span>
          </p>
          <h2 class="name">
            <span v-if="shown.unique" class="unique" :aria-label="$t('browser.common.unique')">◆</span>
            {{ shown.name }}
          </h2>
          <p v-if="shown.subname" class="subname">{{ shown.subname }}</p>
          <p v-if="shown.traits?.length" class="traits">{{ shown.traits.join('. ') }}.</p>
        </header>

        <CardStats :card="shown" />

        <section v-if="shown.text" class="block"><CardText :text="shown.text" /></section>
        <section v-if="shown.backText" class="block block--back">
          <h3 class="block-title">{{ shown.backName ?? $t('browser.card.reverse') }}</h3>
          <CardText :text="shown.backText" />
        </section>
        <p v-if="shown.flavor" class="flavor"><CardText :text="shown.flavor" /></p>

        <dl class="facts">
          <template v-for="fact in facts" :key="fact.label">
            <dt>{{ fact.label }}</dt>
            <dd>{{ fact.value }}</dd>
          </template>
        </dl>
        <p class="links">
          <a class="button button--ghost" :href="marvelcdb" target="_blank" rel="noreferrer">{{ $t('browser.card.viewOnMarvelcdb') }} ↗</a>
        </p>
      </div>
    </article>
  </div>
</template>

<style scoped>
.detail-container { container-type: inline-size; }
.detail { display: grid; grid-template-columns: minmax(0, 21rem) minmax(0, 1fr); gap: var(--space-5); align-items: start; }
.art { position: sticky; top: var(--space-4); display: flex; flex-direction: column; gap: var(--space-3); }
.folding-frame { position: relative; aspect-ratio: 63 / 88; border-radius: var(--radius-card, 6px); }
.flip { display: grid; transform-style: preserve-3d; transform: perspective(1600px) rotateY(0); transition: transform 0.55s ease-in-out; }
.flip.is-flipped { transform: perspective(1600px) rotateY(180deg); }
/* both faces in the one cell, so the box is the larger of the two */
.flip > .side { grid-area: 1 / 1; align-self: center; backface-visibility: hidden; }
.flip > .side--back { transform: rotateY(180deg); }
@media (prefers-reduced-motion: reduce) { .flip { transition: none; } }
.faces { display: flex; flex-wrap: wrap; gap: var(--space-2); }
.line { display: flex; flex-wrap: wrap; align-items: center; gap: var(--space-3); margin: 0; font-size: var(--text-sm); color: var(--c-text-muted); }
.name { display: flex; align-items: baseline; gap: 0.3em; margin: 6px 0 0; font-size: 2.2rem; line-height: 1; text-transform: uppercase; }
.unique { color: var(--chip, var(--c-accent)); font-size: 0.5em; }
.subname { margin: 2px 0 0; color: var(--c-text-muted); font-size: var(--text-lg); }
.traits { margin: var(--space-2) 0 0; font-style: italic; font-weight: 600; color: var(--c-text-muted); font-size: var(--text-sm); text-transform: uppercase; letter-spacing: 0.04em; }
.block { padding: var(--space-3) var(--space-4); border: 1px solid var(--c-border); border-left: 3px solid var(--chip, var(--c-border-strong)); border-radius: var(--radius); background: var(--c-surface); line-height: 1.6; }
.block--back { border-style: dashed; border-left-style: solid; }
.block-title { margin: 0 0 var(--space-2); font-family: var(--font-ui); font-size: var(--text-xs); letter-spacing: 0.08em; text-transform: uppercase; color: var(--c-text-faint); }
.flavor { color: var(--c-text-muted); font-style: italic; font-size: var(--text-sm); }
.facts { display: grid; grid-template-columns: auto 1fr; gap: var(--space-1) var(--space-4); padding-top: var(--space-3); border-top: 1px solid var(--c-border); font-size: var(--text-sm); }
.facts dt { color: var(--c-text-faint); }
.facts dd { margin: 0; }
@container (width < 34rem) {
  .detail { grid-template-columns: minmax(0, 1fr); }
  .art { position: static; max-width: 20rem; margin-inline: auto; width: 100%; }
}
</style>
