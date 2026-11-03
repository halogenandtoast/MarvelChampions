<script setup lang="ts">
import HeroPortrait from './HeroPortrait.vue'
import { useCards } from '../cards'
import { heroFace } from '../deckChoice'
import type { GameView } from '../server'
defineProps<{ competition: NonNullable<GameView['competition']>; shownArea: number | null }>()
const cards = useCards()
const emit = defineEmits<{ show: [area: number] }>()
</script>

<template>
  <nav class="competition-status" :aria-label="$t('civilWar.status')">
    <button v-for="team in competition.teams" :key="team.team" type="button" :class="{ selected: shownArea === team.area }" :aria-pressed="shownArea === team.area" @click="emit('show', team.area)">
      <strong>{{ $t(`civilWar.${team.team.toLowerCase()}`) }}</strong>
      <span class="roster">
        <span v-for="member in team.roster" :key="member.id ?? member.name" class="member" :class="{ eliminated: member.eliminated }">
          <HeroPortrait v-if="member.code" class="hero" :code="member.code" />
          <span>{{ member.name }}<span v-if="member.code" class="hero-name"> · {{ cards.name(heroFace(member.code), member.code) }}</span><span v-if="member.eliminated"> · {{ $t('civilWar.eliminated') }}</span></span>
        </span>
      </span>
      <span class="totals">{{ $t('civilWar.heroHealth') }} {{ team.remainingHeroHitPoints }} · {{ $t('civilWar.schemeStage', { stage: team.advancedMainScheme ? 2 : 1 }) }} · {{ $t('civilWar.mainThreat') }} {{ team.mainThreat }}</span>
      <span v-if="team.outcome" class="result">{{ $t(`civilWar.teamResult.${team.outcome}`) }}</span>
    </button>
  </nav>
</template>

<style scoped>
.competition-status { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 8px; padding: 8px 16px; background: var(--ink-1); }
button { display: grid; gap: 3px; padding: 10px 14px; border: 1px solid var(--line); border-radius: 8px; background: var(--ink-2); color: var(--text); font: inherit; text-align: left; cursor: pointer; }
button.selected { border-color: var(--steel); }
button:hover { background: var(--ink-3); }
strong { color: var(--title); }
.roster { display: flex; flex-wrap: wrap; gap: 6px 16px; }
.member { display: inline-flex; gap: 6px; align-items: center; min-width: 0; }
.member.eliminated { opacity: 0.6; }
.hero { width: 24px; height: 28px; flex: none; }
.hero-name { color: var(--text-muted); }
.totals { font-size: 0.8rem; color: var(--text-muted); }
.result { font-weight: 600; }
@media (max-width: 600px) { .competition-status { padding: 6px; gap: 6px; } button { padding: 8px; } }
</style>
