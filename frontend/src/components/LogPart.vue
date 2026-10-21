<script setup lang="ts">
// One piece of a log entry's sentence. An i18n part renders through <i18n-t>, so a
// translated sentence still carries its card chips.
import { computed } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import GameIcon from '../browser/components/GameIcon.vue'
import type { LogPart, LogRef } from '../gameLog'
import { useCards } from '../cards'

const props = defineProps<{ part: LogPart }>()

const overlay = useCardOverlay()
const cards = useCards()
// a ref the engine could only name by its code is named from the card database
const refName = (ref: LogRef) => (ref.cardCode && ref.name === ref.cardCode ? cards.name(ref.cardCode, ref.name) : ref.name)

const i18nKey = computed(() => (props.part.tag === 'LogI18n' ? props.part.contents[0] : ''))
const i18nVars = computed(() => (props.part.tag === 'LogI18n' ? props.part.contents[1] : {}))
const hasVars = computed(() => Object.keys(i18nVars.value).length > 0)
// a `count` variable picks the plural form, as the server's i18n does
const plural = computed(() => {
  const count = i18nVars.value['count']
  return count?.tag === 'LogNumber' ? count.contents : undefined
})

// a card face down shows its back: an encounter card dealt to a player, a boost card
const show = (ref: LogRef, event: PointerEvent) => {
  if (!ref.cardCode && !ref.faceDown) return
  overlay.show(
    { code: ref.faceDown ? null : ref.cardCode, name: ref.name, back: ref.faceDown ? 'encounter' : null },
    event.currentTarget as HTMLElement,
    false,
    event.pointerType === 'touch',
  )
}
// stats as the cards print them; resources and the like as icons
const words = ['thwart', 'attack', 'defense', 'recover', 'scheme', 'threat', 'health', 'hand']
</script>

<template>
  <span v-if="part.tag === 'LogText'">{{ part.contents }}</span>

  <!-- an i18n part without variables is plain text: one nested in another's slot as a
       component would leave the outer placeholder unfilled -->
  <span v-else-if="part.tag === 'LogI18n' && !hasVars">{{ $t(i18nKey) }}</span>

  <i18n-t v-else-if="part.tag === 'LogI18n'" :keypath="i18nKey" :plural="plural" tag="span" scope="global">
    <template v-for="(value, name) in i18nVars" #[name] :key="name">
      <LogPart :part="value" />
    </template>
  </i18n-t>

  <span
    v-else-if="part.tag === 'LogRefPart'"
    class="log-ref"
    :class="`log-ref--${part.contents.kind}`"
    @pointerenter="(e: PointerEvent) => e.pointerType === 'mouse' && show(part.contents as LogRef, e)"
    @pointerleave="(e: PointerEvent) => e.pointerType === 'mouse' && overlay.hide()"
    @pointerup="(e: PointerEvent) => e.pointerType === 'touch' && show(part.contents as LogRef, e)"
    >{{ refName(part.contents as LogRef) }}</span
  >

  <span v-else-if="part.tag === 'LogNumber'" class="log-num">{{ part.contents }}</span>

  <span v-else-if="part.tag === 'LogDelta'" class="log-delta" :class="part.contents < 0 ? 'log-delta--down' : 'log-delta--up'"
    >{{ part.contents < 0 ? '−' : '+' }}{{ Math.abs(part.contents) }}</span
  >

  <span v-else-if="part.tag === 'LogIcon' && words.includes(part.contents)" class="log-stat" :class="`log-stat--${part.contents}`">{{ $t(`log.stats.${part.contents}`) }}</span>
  <GameIcon v-else-if="part.tag === 'LogIcon'" :name="part.contents" />

  <span v-else-if="part.tag === 'LogList'">
    <template v-for="(item, i) in part.contents" :key="i">
      <span v-if="i > 0">{{ i === part.contents.length - 1 ? $t('log.listLast') : $t('log.listSeparator') }}</span>
      <LogPart :part="item" />
    </template>
  </span>
</template>

<style scoped>
.log-ref { color: var(--text); font-weight: 600; cursor: help; border-bottom: 1px dotted var(--line-strong); }
.log-ref--RefPlayer { color: var(--title); border-bottom: 0; cursor: default; }
.log-ref--RefIdentity, .log-ref--RefAlly, .log-ref--RefSupport, .log-ref--RefUpgrade, .log-ref--RefEvent { color: var(--steel-bright); }
.log-ref--RefVillain, .log-ref--RefMinion { color: #f0959b; }
.log-ref--RefTreachery, .log-ref--RefAttachment, .log-ref--RefObligation { color: #c3a6e6; }
.log-ref--RefMainScheme, .log-ref--RefSideScheme { color: #f2c063; }
.log-ref--RefScenario { color: var(--title); }

.log-num { font-variant-numeric: tabular-nums; font-weight: 600; }

.log-delta { font-variant-numeric: tabular-nums; padding: 0 3px; border-radius: 3px; }
.log-delta--up { color: #9fd8ae; background: rgb(79 174 106 / 0.18); }
.log-delta--down { color: #f0a4a9; background: rgb(214 80 88 / 0.18); }

.log-stat { margin-left: 2px; font-size: 0.85em; font-weight: 800; letter-spacing: 0.04em; color: var(--text-muted); }
.log-stat--threat, .log-stat--scheme { color: var(--amber); }
.log-stat--attack { color: var(--attack); }
.log-stat--thwart, .log-stat--defense { color: var(--steel-bright); }
.log-stat--health, .log-stat--recover { color: var(--health); }
</style>
