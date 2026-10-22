<script setup lang="ts">
import { Bug, Download, FlagTriangleRight, Menu, PanelRightClose, PanelRightOpen, Rewind, RotateCcw, Terminal, Undo2, Layers } from 'lucide-vue-next'
import GameMenu from './GameMenu.vue'
import type { CampaignInfo } from '../server'

const undoShortcut = /Mac|iPhone|iPad/.test(navigator.platform) ? '⌘Z' : 'Ctrl+Z'

// a guest at a friend's game can undo and export, but restarting and debugging are the owner's
const { round = null, phase = null } = defineProps<{
  canUndo: boolean
  showSidebar: boolean
  spectate: boolean
  guest?: boolean
  round?: number | null
  phase?: string | null
  /** whose turn it is, by name */
  turn?: string | null
  name?: string | null
  /** the campaign this game is a scenario of */
  campaign?: CampaignInfo | null
}>()
const emit = defineEmits<{
  reset: []
  undo: [to?: 'phase' | 'round']
  export: []
  fileBug: []
  debug: []
  toggleSidebar: []
}>()
const phases = ['PlayerPhase', 'VillainPhase']
</script>

<template>
  <div class="game-bar">
    <div class="left">
      <span v-if="spectate" class="spectating">{{ $t('game.bar.spectating') }}</span>
      <template v-else>
        <GameMenu>
          <Menu aria-hidden="true" />
          {{ $t('common.game') }}
          <template #items>
            <template v-if="!guest">
              <div class="menu-section">{{ $t('common.scenario') }}</div>
              <button type="button" @click="emit('reset')"><FlagTriangleRight aria-hidden="true" />{{ $t('game.bar.restartScenario') }}</button>
            </template>
            <div class="menu-section">{{ $t('game.bar.file') }}</div>
            <button type="button" @click="emit('export')"><Download aria-hidden="true" />{{ $t('game.bar.exportGame') }}</button>
            <template v-if="!guest">
              <div class="menu-section">{{ $t('game.bar.testing') }}</div>
              <button type="button" @click="emit('debug')"><Terminal aria-hidden="true" />{{ $t('game.bar.debugMessage') }}</button>
            </template>
          </template>
        </GameMenu>
        <GameMenu>
          <Undo2 aria-hidden="true" />
          {{ $t('game.bar.undo') }}
          <template #items>
            <button type="button" :disabled="!canUndo" @click="emit('undo')">
              <Undo2 aria-hidden="true" />{{ $t('game.bar.undoLastAnswer') }}<span class="shortcut">{{ undoShortcut }}</span>
            </button>
            <div class="menu-section">{{ $t('game.bar.undoTo') }}</div>
            <button type="button" :disabled="!canUndo" @click="emit('undo', 'phase')"><Layers aria-hidden="true" />{{ $t('game.bar.startOfPhase') }}</button>
            <button type="button" :disabled="!canUndo" @click="emit('undo', 'round')"><Rewind aria-hidden="true" />{{ $t('game.bar.startOfRound') }}</button>
          </template>
        </GameMenu>
        <button type="button" class="bar-button" @click="emit('fileBug')"><Bug aria-hidden="true" />{{ $t('game.bar.fileBug') }}</button>
      </template>
    </div>

    <div v-if="round !== null" class="tracker" :aria-label="$t('game.phaseTracker')">
      <span v-if="campaign" class="campaign" :title="$t(`campaign.${campaign.mode}`)">
        {{ campaign.name ?? $t('campaign.riseOfRedSkull') }} · {{ $t('campaign.scenarioOf', { n: campaign.scenario, total: campaign.scenarios }) }} · {{ $t(`campaign.${campaign.mode}`) }}
      </span>
      <span v-if="name" class="game-name">{{ name }}</span>
      <span class="round"><RotateCcw aria-hidden="true" />{{ $t('game.round', { round }) }}</span>
      <ol class="phases">
        <li v-for="p in phases" :key="p" :class="[p, { current: p === phase }]" :aria-current="p === phase ? 'step' : undefined">
          {{ $t(`game.phases.${p}`) }}
        </li>
      </ol>
      <span v-if="turn && phase === 'PlayerPhase'" class="turn">{{ $t('game.turnOf', { name: turn }) }}</span>
    </div>

    <div class="right">
      <button
        type="button"
        class="bar-button"
        :aria-label="$t('game.bar.toggleSidebar')"
        :title="$t('game.bar.toggleSidebar')"
        :aria-expanded="showSidebar"
        @click="emit('toggleSidebar')"
      >
        <PanelRightClose v-if="showSidebar" aria-hidden="true" />
        <PanelRightOpen v-else aria-hidden="true" />
      </button>
    </div>
  </div>
</template>

<style scoped>
.game-bar {
  display: grid;
  grid-template-columns: 1fr auto 1fr;
  align-items: stretch;
  height: 40px;
  background: var(--ink-2);
  border-bottom: 1px solid var(--line);
  font-size: 0.85rem;
  flex-shrink: 0;
  position: relative;
  z-index: 50;
}
.left, .right { display: flex; align-items: stretch; }
.campaign { display: flex; align-items: center; padding: 0 10px; color: var(--amber); font-weight: 700; white-space: nowrap; }
.right { justify-content: flex-end; }
.game-bar :deep(svg) { width: 15px; height: 15px; }
.spectating { display: flex; align-items: center; padding: 0 14px; color: var(--text-muted); text-transform: uppercase; letter-spacing: 0.1em; font-size: 0.75em; font-weight: 700; }
.menu-section { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 1.5px; color: var(--menu-section-text); padding: 8px 12px 4px; user-select: none; pointer-events: none; }
.bar-button {
  background: none;
  border: 0;
  color: inherit;
  font: inherit;
  padding: 0 12px;
  display: flex;
  gap: 6px;
  align-items: center;
  cursor: pointer;
}
.bar-button:hover { background: var(--hover-tint); }
.game-bar :deep(.menu-items) { border-radius: 0 0 10px 10px; overflow: hidden; }
.game-bar :deep(.menu-items button) {
  background: none;
  border: 0;
  color: inherit;
  font: inherit;
  padding: 9px 12px;
  display: flex;
  gap: 9px;
  align-items: center;
  cursor: pointer;
  text-align: left;
}
.game-bar :deep(.menu-items button:hover:not(:disabled)) { background: var(--hover-tint); }
.game-bar :deep(.menu-items button:disabled) { opacity: 0.4; cursor: not-allowed; }
.shortcut { margin-left: auto; padding-left: 16px; font-size: 0.8em; color: var(--text-faint); }

.tracker { display: flex; align-items: center; gap: 14px; padding: 0 12px; white-space: nowrap; }
.game-name { font-family: var(--font-display); font-weight: 700; font-size: 1.05rem; color: var(--text-muted); max-width: 18vw; overflow: hidden; text-overflow: ellipsis; }
.round { display: flex; align-items: center; gap: 6px; font-family: var(--font-display); font-size: 1.05rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.03em; }
.phases { display: flex; margin: 0; padding: 2px; list-style: none; border-radius: 8px; background: var(--ink-0); border: 1px solid var(--line); }
.phases li { padding: 3px 10px; border-radius: 6px; font-size: 0.68rem; font-weight: 700; letter-spacing: 0.1em; text-transform: uppercase; color: var(--text-faint); }
.phases li.current.PlayerPhase { background: var(--steel-deep); color: var(--steel-bright); }
.phases li.current.VillainPhase { background: var(--crimson-deep); color: #ffb3b7; }
.turn { font-size: 0.78rem; color: var(--text-muted); }
@media (width < 1100px) {
  .game-name, .turn { display: none; }
}
@media (width < 760px) {
  .game-bar { grid-template-columns: 1fr auto; }
  .tracker { display: none; }
}
</style>
