<script setup lang="ts">
// Everything known about one card: the scan, its statistics, its rules text, and its
// other face, for an identity's alter-ego or a scheme's B side.
import { localRulebookUrl } from '../../rulebooks'
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
const horizontal = computed(() => third.value?.orientation === 'landscape')
// faces sharing a name (Ant-Man's two hero forms) are one tab, told apart by the trait
// naming the form, printed last
const faceGroups = computed(() => {
  const groups: { name: string; faces: { card: Card; i: number }[] }[] = []
  faces.value.forEach((card, i) => {
    const group = groups.find((g) => g.name === card.name)
    if (group) group.faces.push({ card, i })
    else groups.push({ name: card.name, faces: [{ card, i }] })
  })
  return groups
})
// a scheme's sides by stage (1A, 1B), else by the code's side letter (side schemes print none)
const faceLabel = (c: Card, group: { faces: { card: Card }[] }) =>
  group.faces.some((f) => /scheme$/.test(f.card.type))
    ? /^\d+[AB]$/.test(c.stage ?? '') ? c.stage! : c.code.slice(-1).toUpperCase()
    : (c.traits?.at(-1) ?? (/[ab]$/.test(c.code) ? c.code.slice(-1).toUpperCase() : c.name))

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
const source = computed(() => localRulebookUrl(shown.value.sourceUrl ?? `https://marvelcdb.com/card/${shown.value.code}`))
</script>

<template>
  <div class="detail-container">
    <article class="detail" :data-aspect="shown.faction">
      <div class="art">
        <div v-if="third" :key="card.code" class="folding-frame" :class="{ unfolded: shown === third, horizontal }"><FoldingIdentity :faces="faces.map((c) => c.code)" :code="shown.code" /></div>
        <!-- a two-sided card turns over to its other face -->
        <div v-else-if="other" :key="card.code" class="flip" :class="{ 'is-flipped': imageIndex === 1 }">
          <CardImage class="side" :card="card" eager />
          <CardImage class="side side--back" :card="other" eager />
        </div>
        <CardImage v-else :card="shown" eager />
        <div v-if="other" class="faces" role="tablist" :aria-label="$t('browser.card.faces')">
          <template v-for="group in faceGroups" :key="group.name">
            <!-- one hero, two forms (Ant-Man): one name, its forms segmented -->
            <div v-if="group.faces.length > 1" class="split" :class="{ 'is-active': group.faces.some((f) => f.i === imageIndex) }">
              <span class="split-name">{{ group.name }}</span>
              <button
                v-for="f in group.faces"
                :key="f.card.code"
                type="button"
                class="button"
                role="tab"
                :aria-selected="imageIndex === f.i"
                :class="{ 'is-active': imageIndex === f.i }"
                @click="imageIndex = f.i"
              >
                {{ faceLabel(f.card, group) }}
              </button>
            </div>
            <button
              v-else
              type="button"
              class="button"
              role="tab"
              :aria-selected="imageIndex === group.faces[0].i"
              :class="{ 'is-active': imageIndex === group.faces[0].i }"
              @click="imageIndex = group.faces[0].i"
            >
              {{ group.name }}
            </button>
          </template>
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

        <section v-if="shown.text" class="block">
          <p v-if="!shown.textVerified && shown.ocr && !shown.ocr.verified" class="flavor">{{ $t('browser.card.unverifiedOcr') }}</p>
          <CardText :text="shown.text" />
        </section>
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
          <a class="button button--ghost" :href="source" target="_blank" rel="noreferrer">{{ $t(shown.sourceUrl ? 'browser.card.viewSource' : 'browser.card.viewOnMarvelcdb') }} ↗</a>
        </p>
      </div>
    </article>
  </div>
</template>

<style scoped>
.detail-container { container-type: inline-size; }
.detail { display: grid; grid-template-columns: minmax(0, 21rem) minmax(0, 1fr); gap: var(--space-5); align-items: start; }
.art { position: sticky; top: var(--space-4); display: flex; flex-direction: column; gap: var(--space-3); container-type: inline-size; }
/* Ant-Man at true sizes: the giant, a sideways card wide and two tall, fills the column,
   so the shut card is 63/88 of it */
.folding-frame { --book-w: 71.59cqi; position: relative; width: 71.59cqi; aspect-ratio: 63 / 88; border-radius: var(--radius-card, 6px); transition: width 0.4s ease, aspect-ratio 0.4s ease; }
.folding-frame.unfolded { width: 100cqi; aspect-ratio: 88 / 126; }
.folding-frame.horizontal { --book-w: 50cqi; width: 50cqi; }
.folding-frame.horizontal.unfolded { width: 100cqi; aspect-ratio: 126 / 88; }
.flip { display: grid; transform-style: preserve-3d; transform: perspective(1600px) rotateY(0); transition: transform 0.4s ease-in-out; }
.flip.is-flipped { transform: perspective(1600px) rotateY(180deg); }
/* both faces in the one cell, so the box is the larger of the two */
.flip > .side { grid-area: 1 / 1; align-self: center; backface-visibility: hidden; }
.flip > .side--back { transform: rotateY(180deg); }
@media (prefers-reduced-motion: reduce) { .flip { transition: none; } }
.split { display: inline-flex; align-items: center; gap: 2px; padding: 2px 2px 2px var(--space-3); border: 1px solid var(--c-border); border-radius: var(--radius); background: var(--c-surface); }
.split.is-active { border-color: var(--c-accent); }
.split-name { margin-right: var(--space-1); font-weight: 600; }
.split .button { padding: 0.25rem 0.5rem; border-color: transparent; font-size: var(--text-sm); }
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
