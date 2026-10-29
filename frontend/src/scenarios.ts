// How the scenarios, their releases and the modular sets are named and drawn, for choosing
// one and for the bar that says what was chosen.
import { computed, type Ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useCards } from './cards'
import type { ScenarioOption } from './server'

// Releases, scenarios and modular sets are named by the card data, so a new box needs only
// its cards: a scenario by its villain's card, a modular set by any card in it.
export interface Release { code: string; name: string; order: number }
const NO_RELEASE: Release = { code: '', name: '', order: Infinity }
// the engine's BombScare is MarvelCDB's bomb_scare, save for the sets MarvelCDB abbreviates
const SET_CODES: Record<string, string> = { ExperimentalWeapons: 'exper_weapon', WeaponMaster: 'weap_master', MasterOfTime: 'mot', ExpertKang: 'exp_kang', ExpertCampaign: 'expcamp', KreeMilitants: 'kree_militant' }
const setCode = (set: string) => SET_CODES[set] ?? set.replace(/(?<!^)([A-Z])/g, '_$1').toLowerCase()
// a scenario set up by its main scheme, as The Wrecking Crew is, shows its villains
const SCENARIO_VILLAINS: Record<string, string[]> = {
  '07001': ['07002', '07017', '07032', '07046'],
  '21098': ['21092'],
  '21160': ['21163'],
}
// a scenario's variants, as Breakout's, are one scenario
export const baseCode = (code: string) => code.replace(/[ab]$/, '')

export const useScenarios = (scenarios: Ref<ScenarioOption[]>) => {
  const { t } = useI18n()
  const cards = useCards()

  const releaseOf = (code: string | null | undefined, card = cards.card(code)): Release =>
    card ? { code: card.packCode, name: card.packName ?? card.packCode, order: card.packOrder ?? Infinity } : NO_RELEASE
  const setCards = computed(() => {
    const bySet = new Map<string, (typeof cards.store.all)[number]>()
    for (const c of cards.store.all) if (c.setCode && !bySet.has(c.setCode)) bySet.set(c.setCode, c)
    return bySet
  })
  const modularName = (set: string) => setCards.value.get(setCode(set))?.setName ?? set
  const modularRelease = (set: string) => releaseOf(null, setCards.value.get(setCode(set)))

  const portraits = (code: string) => SCENARIO_VILLAINS[baseCode(code)] ?? [code]
  const art = (code: string) => cards.sources(code).map((s) => `url('${s}')`).join(', ')

  // a scenario set up by its main scheme's A side is that scenario's extreme variant (The
  // Wrecking Crew: each version A villain over its version B); by its B side, the players
  // choose each villain's version
  const variant = (code: string) =>
    cards.card(code)?.type === 'main_scheme' ? (/a$/.test(code) ? 'extreme' : /b$/.test(code) ? 'mixed' : null) : null
  // a scenario pack of one scenario, as The Wrecking Crew is, is named for the pack; a box of
  // several, as the core set or The Green Goblin, names each for its villain
  const scenarioCount = computed(() => {
    const count = new Map<string, Set<string>>()
    for (const o of scenarios.value) count.set(releaseOf(o.code).code, (count.get(releaseOf(o.code).code) ?? new Set()).add(baseCode(o.code)))
    return count
  })
  const ownPack = (code: string) => scenarioCount.value.get(releaseOf(code).code)?.size === 1
  // a tile names the scenario: the pack of one, or its set, which is its villain's but for
  // a villain with several, as the Green Goblin's Risky Business and Mutagen Formula
  const tileName = (code: string) => (ownPack(code) ? releaseOf(code).name : (cards.card(code)?.setName ?? cards.name(code, code)))
  const scenarioName = (code: string) => {
    const name = tileName(code)
    const v = variant(code)
    return v === 'extreme' ? t('newGame.extremeVariant', { scenario: name }) : v === 'mixed' ? t('newGame.mixedVariant', { scenario: name }) : name
  }
  const villainName = (code: string) => (ownPack(code) ? releaseOf(code).name : cards.name(code, code))
  // what the bar over the settings and the lobby says of a scenario, past its name
  const details = (setup: { scenarioCode: string; modulars: string[]; difficulty: 'Normal' | 'Expert' }) =>
    [releaseOf(setup.scenarioCode).name, ...setup.modulars.map(modularName)]
      .filter((d) => d !== tileName(setup.scenarioCode))
      .concat(t(`difficulty.${setup.difficulty}`))

  return { releaseOf, modularName, modularRelease, portraits, art, variant, tileName, scenarioName, villainName, details }
}
