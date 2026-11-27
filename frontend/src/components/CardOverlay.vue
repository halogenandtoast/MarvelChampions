<script setup lang="ts">
import { assetUrl } from '../assets'
import { useI18n } from 'vue-i18n'
import { computed, useTemplateRef, watch } from 'vue'
import { useElementSize } from '@vueuse/core'
import { DETAILS_WIDTH, STATUS_WIDTH, useCardOverlay } from '../composables/useCardOverlay'
import { useCards } from '../cards'
import CardFace from './CardFace.vue'
import CardText from '@browser/components/CardText.vue'

const { card, sideways, side, placement, visible, teleportTarget, setBelow, details, detailsBelow, pinned } = useCardOverlay()
const { t } = useI18n()
const { card: findCard, store } = useCards()
const detailCard = computed(() => card.value?.code && !card.value.back ? findCard(card.value.code) : undefined)
const showDetails = computed(() => details.value && !!detailCard.value)
// The Rules Reference errata for the card shown, under it, as its printing may differ (#14).
const errata = computed(() => (card.value && !card.value.back ? store.errataFor(card.value.code) : undefined))
const errataPanel = useTemplateRef<HTMLElement>('errataPanel')
const detailsPanel = useTemplateRef<HTMLElement>('detailsPanel')
const { height: errataHeight } = useElementSize(errataPanel, undefined, { box: 'border-box' })
const { height: detailsHeight } = useElementSize(detailsPanel, undefined, { box: 'border-box' })
const GAP = 8
const contentBelow = computed(() => {
  const errataBelow = errata.value && visible.value ? errataHeight.value + GAP : 0
  if (!showDetails.value || !visible.value) return errataBelow
  if (detailsBelow.value) return detailsHeight.value + GAP + errataBelow
  return Math.max(errataBelow, detailsHeight.value - placement.height)
})
watch(contentBelow, (height) => setBelow(height), { immediate: true })
const statusGroups = computed(() => [...new Set(card.value?.statuses ?? [])].map(status => ({ status, count: card.value!.statuses!.filter(value => value === status).length })))
const detailsStyle = computed(() => ({
  width: detailsBelow.value ? '100%' : `${DETAILS_WIDTH * placement.scale}px`,
  fontSize: `${14 * placement.scale}px`,
  '--details-offset': `${10 + (card.value?.statuses?.length ? STATUS_WIDTH + 10 : 0)}px`,
}))
const errataStyle = computed(() => ({
  top: `calc(100% + ${(detailsBelow.value && showDetails.value ? detailsHeight.value + GAP : 0) + GAP}px)`,
}))
</script>

<template>
  <Teleport :to="teleportTarget">
    <div
      v-if="visible && card"
      class="card-overlay"
      :class="{ sideways, pinned }"
      :style="{ top: `${placement.top}px`, left: `${placement.left}px`, width: `${placement.width}px`, height: `${placement.height}px` }"
      data-card-overlay
    >
      <div class="face"><CardFace :code="card.code" :back="card.back ?? null" :name="card.name" eager /></div>
      <!-- its status cards, on the side away from the card hovered; ponytail: not scaled down with a pinned preview on a small screen -->
      <div v-if="card.statuses?.length" class="statuses" :class="side" :style="{ width: `${STATUS_WIDTH}px` }">
        <div v-for="group in statusGroups" :key="group.status" class="status-group" :class="{ multiple: group.count > 1 }">
          <img v-for="n in group.count" :key="n" :src="assetUrl(`/images/tokens/${group.status}.avif`)" :class="{ inactive: card.inactiveStatuses?.includes(group.status) }" :alt="card.inactiveStatuses?.includes(group.status) ? t('game.status.inactive', { status: t(`game.status.${group.status}`) }) : t(`game.status.${group.status}`)" />
        </div>
      </div>
      <aside
        v-if="showDetails && detailCard"
        ref="detailsPanel"
        class="details"
        :class="[side, { below: detailsBelow }]"
        :style="detailsStyle"
        :data-card-code="detailCard.code"
      >
        <header>
          <p class="kind">{{ detailCard.factionName ?? detailCard.faction }} · {{ detailCard.typeName ?? detailCard.type }}</p>
          <h2><span v-if="detailCard.unique" class="unique" :aria-label="t('browser.common.unique')">◆</span>{{ detailCard.name }}</h2>
          <p v-if="detailCard.traits?.length" class="traits">{{ detailCard.traits.join('. ') }}.</p>
        </header>
        <section v-if="detailCard.text" class="rules"><CardText :text="detailCard.text" /></section>
        <section v-if="detailCard.backText" class="rules back-rules">
          <h3>{{ detailCard.backName ?? t('browser.card.reverse') }}</h3>
          <CardText :text="detailCard.backText" />
        </section>
        <p v-if="detailCard.flavor" class="flavor"><CardText :text="detailCard.flavor" /></p>
        <footer>{{ detailCard.code }}</footer>
      </aside>
      <aside v-if="errata" ref="errataPanel" class="errata" :style="errataStyle">
        <header>
          <span class="errata-title">{{ t('game.errata.title') }}</span>
          <span class="errata-source">{{ t('game.errata.source', { page: errata.page }) }}</span>
        </header>
        <CardText :text="errata.text" />
      </aside>
    </div>
  </Teleport>
</template>

<style scoped>
.card-overlay {
  position: fixed;
  z-index: 9999;
  pointer-events: none;
  animation: overlay-in 0.12s ease-out;
}
.face {
  width: 100%;
  height: 100%;
  border-radius: 14px;
  overflow: hidden;
  box-shadow: 0 18px 50px var(--shadow-strong), 0 0 0 1px rgb(255 255 255 / 0.12);
  background: var(--ink-3);
}
.statuses { position: absolute; top: 0; display: flex; flex-direction: column; gap: 10px; }
.statuses.right { left: calc(100% + 10px); }
.statuses.left { right: calc(100% + 10px); }
.statuses img { display: block; width: 100%; height: auto; border-radius: 10px; box-shadow: 0 12px 30px var(--shadow-strong), 0 0 0 1px rgb(255 255 255 / 0.12); }
.status-group { display: flex; }
.status-group.multiple img { width: 62.5%; flex: none; }
.status-group.multiple img + img { margin-left: -25%; }
.statuses img.inactive { filter: grayscale(1); }
.details {
  position: absolute;
  top: 0;
  box-sizing: border-box;
  max-height: calc(100vh - 20px);
  overflow-y: auto;
  padding: 14px 16px;
  border: 1px solid var(--line-strong);
  border-left: 3px solid var(--steel);
  border-radius: 12px;
  background: var(--surface-1);
  color: var(--text);
  line-height: 1.55;
  scrollbar-width: thin;
  box-shadow: 0 18px 50px var(--shadow-strong);
}
.details.right { left: calc(100% + var(--details-offset)); }
.details.left { right: calc(100% + var(--details-offset)); }
.card-overlay.pinned .details { pointer-events: auto; }
.details.below {
  top: calc(100% + 10px);
  right: auto;
  left: 0;
  max-height: 32vh;
}
.details header { margin-bottom: 10px; }
.kind { margin: 0; color: var(--text-muted); font-size: 0.82em; }
.details h2 { display: flex; align-items: baseline; gap: 6px; margin: 4px 0 0; font-size: 1.65em; line-height: 1.15; }
.unique { color: var(--steel-bright); font-size: 0.65em; }
.traits { margin: 5px 0 0; color: var(--text-muted); font-size: 0.86em; font-style: italic; }
.rules { padding-top: 10px; border-top: 1px solid var(--line); }
.back-rules { margin-top: 10px; }
.back-rules h3 { margin: 0 0 6px; font-size: 0.9em; color: var(--text-muted); }
.flavor { margin: 10px 0 0; color: var(--text-muted); font-size: 0.88em; font-style: italic; }
.details footer { margin-top: 10px; padding-top: 6px; border-top: 1px solid var(--line); color: var(--text-faint); font-size: 0.75em; }
.errata {
  position: absolute;
  left: 0;
  width: 100%;
  box-sizing: border-box;
  padding: 9px 12px 10px;
  border-radius: 10px;
  border-left: 3px solid var(--amber);
  background: var(--surface-2);
  color: var(--text);
  font-size: 12.5px;
  line-height: 1.4;
  box-shadow: 0 12px 30px var(--shadow-strong), 0 0 0 1px rgb(255 255 255 / 0.12);
}
.errata header { display: flex; justify-content: space-between; align-items: baseline; gap: 8px; margin-bottom: 4px; }
.errata-title { font-family: var(--font-display); font-size: 13px; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; color: var(--amber); }
.errata-source { font-size: 11px; color: var(--text-muted); }
@keyframes overlay-in {
  from { opacity: 0; transform: scale(0.97); }
  to { opacity: 1; transform: none; }
}
</style>
