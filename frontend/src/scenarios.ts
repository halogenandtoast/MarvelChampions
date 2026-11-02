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
const SET_CODES: Record<string, string> = { ExtremeRiskSet: 'extreme_risk', GrowingStrongSet: 'growing_strong', TechnoSet: 'techno', WhiteoutSet: 'whiteout', AosAimAbduction: 'a.i.m._abduction', AosAimScience: 'a.i.m._science', AosBatrocsBrigade: 'batrocs_brigade', AosScientistSupreme: 'scientist_supreme', AosGravitationalPull: 'gravitational_pull', AosHardSound: 'hard_sound', AosPaleLittleSpider: 'pale_little_spider', AosPowerOfTheAtom: 'power_of_the_atom', AosSupersonic: 'supersonic', AosTheLeaper: 'the_leaper', SauronSet: 'sauron', ArcadeSet: 'arcade', CrazyGangSet: 'crazy_gang', HellfireSet: 'hellfire', StandardIII: 'standard_iii', AoaUnus: 'unus', AoaInfinites: 'infinites', AoaDystopianNightmare: 'dystopian_nightmare', AoaFourHorsemen: 'four_horsemen', AoaHounds: 'hounds', AoaApocalypse: 'apocalypse', AoaPrelates: 'prelates', AoaDarkRiders: 'dark_riders', AoaDarkBeast: 'dark_beast', AoaBlueMoon: 'blue_moon', AoaGenosha: 'genosha', AoaSavageLand: 'savage_land', AoaEnSabahNur: 'en_sabah_nur', AoaCelestialTech: 'celestial_tech', AoaClanAkkaba: 'clan_akkaba', CrossfiresCrew: 'crossfire_crew', MisterHydeSet: 'mister_hyde', WreckingCrewModular: 'wrecking_crew_modular', StandardII: 'standard_ii', ExpertII: 'expert_ii', ShadowKingSet: 'shadow_king', ZzzaxSet: 'zzzax', BlackOrderSet: 'black_order', ExperimentalWeapons: 'exper_weapon', WeaponMaster: 'weap_master', MasterOfTime: 'mot', ExpertKang: 'exp_kang', ExpertCampaign: 'expcamp', KreeMilitants: 'kree_militant', ShieldTech: 'shield_tech', IronSpiderSinisterSix: 'ironspider_sinister', LadyDeathstrikeSet: 'deathstrike', ExodusSet: 'exodus', ReaversSet: 'reavers', SciFi: 'sci-fi', LongshotSet: 'longshot', NeMarauders: 'marauders', NeJuggernaut: 'juggernaut', NeStryfe: 'stryfe', HopeSummersSet: 'hope_summers', FlightSet: 'flight', SuperStrengthSet: 'super_strength', TelepathySet: 'telepathy', NextEvolutionCampaign: 'next_evol_campaign' }
const setCode = (set: string) => SET_CODES[set] ?? set.replace(/(?<!^)([A-Z])/g, '_$1').toLowerCase()
// Product rulebooks, paired with the summaries under newGame.modularRules.
// The scenario's named recommendations remain the server's printed setup contents.
const RULEBOOK_ROOT = 'https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/'
const MODULAR_RULEBOOKS: Record<string, string> = {
  aoa: `${RULEBOOK_ROOT}mc45_age_of_apocalypse_rulebook.pdf`,
  mojo: `${RULEBOOK_ROOT}mc39_mojomania_rules_insert-compressed.pdf`,
  core: `${RULEBOOK_ROOT}mvc01_learn_to_play_eng-compressed.pdf`,
  gob: `${RULEBOOK_ROOT}mc02_green_goblin_rules_insert.pdf`,
  twc: `${RULEBOOK_ROOT}mc03_wrecking_crew_rules_insert-compressed.pdf`,
  trors: `${RULEBOOK_ROOT}mc10_the_rise_of_red_skull_rules_web.pdf`,
  toafk: `${RULEBOOK_ROOT}mc11_once_and_future_kang_rules_insert-compressed.pdf`,
  gmw: `${RULEBOOK_ROOT}mc16_galaxys_most_wanted_rules_website-compressed.pdf`,
  mts: `${RULEBOOK_ROOT}mc21_the_mad_titans_shadow_rulebook-compressed.pdf`,
  sm: `${RULEBOOK_ROOT}mc27_sinister_motives_rules_v5-compressed.pdf`,
  next_evol: `${RULEBOOK_ROOT}mc40_next_evolution_rulebook-web.pdf`,
  mut_gen: `${RULEBOOK_ROOT}mc32_mutant_genesis_rulebook_v5-compressed.pdf`,
  hood: 'https://images-cdn.fantasyflightgames.com/filer_public/89/66/89666652-cfcb-47c1-822f-a7a377a69401/mc24_the_hood_rules_insert.pdf',
}
// a scenario set up by its main scheme, as The Wrecking Crew is, shows its villains
const SCENARIO_VILLAINS: Record<string, string[]> = {
  '45081': ['45081a', '45082a', '45083a', '45084a'],
  '40077': ['40080'],
  '40103': ['40102'],
  '07001': ['07002', '07017', '07032', '07046'],
  '21098': ['21092'],
  '21160': ['21163'],
  '27100': ['27094', '27095', '27096', '27097', '27098', '27099'],
  '32121': ['32130'],
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
  const modularRules = (code: string) => {
    const release = releaseOf(code)
    return {
      product: release.name,
      text: t(`newGame.modularRules.${MODULAR_RULEBOOKS[release.code] ? release.code : 'other'}`),
      source: MODULAR_RULEBOOKS[release.code],
      usesModulars: release.code !== 'twc',
    }
  }
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

  return { releaseOf, modularRules, modularName, modularRelease, portraits, art, variant, tileName, scenarioName, villainName, details }
}
