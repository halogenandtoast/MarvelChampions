<script setup lang="ts">
import { onBeforeUnmount, onMounted, useTemplateRef } from 'vue'
import { X, Undo2, ShoppingBag } from 'lucide-vue-next'
import { useCards } from '../cards'
import CardFace from './CardFace.vue'
import { useCardOverlay } from '../composables/useCardOverlay'

const props = defineProps<{
  available: [string, number][]
  owned: string[]
  units: number
  working: boolean
  error: string | null
}>()
const emit = defineEmits<{ buy: [code: string]; undo: [code: string]; close: [] }>()
const cards = useCards()
const dialog = useTemplateRef<HTMLDialogElement>('dialog')
const overlay = useCardOverlay()
const preview = (code: string, target: EventTarget | null, pinned = false) => {
  if (target instanceof HTMLElement) overlay.show({ code, name: cards.name(code, code) }, target, undefined, pinned)
}
onMounted(() => dialog.value?.showModal())
onBeforeUnmount(overlay.hide)
</script>

<template>
  <dialog ref="dialog" class="market-dialog" aria-labelledby="market-title" @close="overlay.hide(); emit('close')" @click="($event.target === dialog) && dialog?.close()">
    <header>
      <div><h2 id="market-title"><ShoppingBag :size="24" />{{ $t('campaign.theMarket') }}</h2></div>
      <button type="button" class="close" :aria-label="$t('campaign.marketClose')" @click="dialog?.close()"><X :size="24" /></button>
    </header>
    <div class="balance"><span>{{ $t('campaign.units') }}</span><strong aria-live="polite">{{ units }}</strong></div>
    <p v-if="error" class="error" role="alert">{{ error }}</p>
    <div class="market-body" :aria-busy="working">
      <section>
        <h3>{{ $t('campaign.marketBrowse') }} <span>{{ available.length }}</span></h3>
        <ul class="card-grid">
          <li v-for="[code, price] in available" :key="code">
            <button type="button" class="card-image" :aria-label="cards.name(code, code)" :data-card-code="code"
              @pointerenter="$event.pointerType === 'mouse' && preview(code, $event.currentTarget)"
              @pointerleave="$event.pointerType === 'mouse' && overlay.hide()"
              @focus="preview(code, $event.currentTarget)" @blur="overlay.hide()"
              @click="preview(code, $event.currentTarget, true)" @keydown.esc.stop.prevent="overlay.hide()">
              <CardFace :code="code" :name="cards.name(code, code)" />
            </button>
            <button type="button" class="btn purchase" :disabled="working || price > units" :aria-label="$t('campaign.marketBuyNamed', { card: cards.name(code, code), count: price }, price)" @click="overlay.hide(); emit('buy', code)">{{ $t('campaign.marketBuy', { count: price }, price) }}</button>
          </li>
        </ul>
      </section>
      <section v-if="props.owned.length" class="owned">
        <h3>{{ $t('campaign.marketCards') }} <span>{{ props.owned.length }}</span></h3>
        <ul class="card-grid">
          <li v-for="code in props.owned" :key="code">
            <button type="button" class="card-image" :aria-label="cards.name(code, code)" :data-card-code="code"
              @pointerenter="$event.pointerType === 'mouse' && preview(code, $event.currentTarget)"
              @pointerleave="$event.pointerType === 'mouse' && overlay.hide()"
              @focus="preview(code, $event.currentTarget)" @blur="overlay.hide()"
              @click="preview(code, $event.currentTarget, true)" @keydown.esc.stop.prevent="overlay.hide()">
              <CardFace :code="code" :name="cards.name(code, code)" />
            </button>
            <button type="button" class="btn ghost purchase" :disabled="working" :aria-label="$t('campaign.marketUndoNamed', { card: cards.name(code, code) })" @click="overlay.hide(); emit('undo', code)"><Undo2 :size="16" />{{ $t('campaign.marketUndo') }}</button>
          </li>
        </ul>
      </section>
    </div>
  </dialog>
</template>

<style scoped>
.market-dialog { width: min(900px, calc(100vw - 32px)); max-height: calc(100dvh - 32px); padding: 0; border: 1px solid var(--text-faint); border-radius: 16px; background: var(--surface-1); color: var(--text); box-shadow: 0 24px 80px var(--shadow-strong); overflow: auto; }
.market-dialog::backdrop { background: rgb(0 0 0 / .75); }
header { display: flex; justify-content: space-between; align-items: flex-start; gap: 24px; padding: 24px 28px 16px; }
h2 { display: flex; align-items: center; gap: 12px; margin: 0; font-family: var(--font-display); font-size: 1.8rem; }
.close { display: grid; place-items: center; min-width: 44px; min-height: 44px; border: 0; border-radius: 8px; background: var(--surface-3); color: var(--text); cursor: pointer; }
.balance { display: flex; justify-content: space-between; align-items: center; padding: 12px 28px; background: var(--surface-2); }
.balance strong { font-size: 1.7rem; font-variant-numeric: tabular-nums; }
.market-body { display: grid; gap: 28px; padding: 20px 28px 28px; }
h3 { display: flex; justify-content: space-between; gap: 12px; font-size: .9rem; margin: 0 0 16px; color: var(--text-muted); }
.card-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(145px, 1fr)); gap: 24px 16px; list-style: none; margin: 0; padding: 0; }
.card-grid li { display: flex; flex-direction: column; gap: 10px; min-width: 0; }
.card-image { display: block; width: 100%; aspect-ratio: 300 / 419; padding: 0; border: 0; border-radius: 10px; overflow: hidden; background: var(--surface-2); box-shadow: 0 4px 12px var(--shadow-strong); cursor: zoom-in; }
.card-image:hover { outline: 2px solid var(--steel-bright); outline-offset: 3px; }
.purchase { display: flex; align-items: center; justify-content: center; gap: 8px; width: 100%; min-height: 44px; font-size: .8rem; }
.owned { padding-top: 24px; border-top: 1px solid var(--text-faint); }
.error { margin: 12px 28px 0; }
button:focus-visible { outline: 2px solid var(--steel-bright); outline-offset: 2px; }
@media (max-width: 640px) { header { padding: 20px 16px 16px; } .balance { padding-inline: 16px; } .market-body { padding: 16px; } .card-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 20px 12px; } }
</style>
