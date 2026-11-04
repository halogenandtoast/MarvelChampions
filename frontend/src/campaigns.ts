import type { CampaignKind, CampaignMode } from './server'

export const campaignArt: Record<CampaignKind, string> = {
  RiseOfRedSkull: '/images/products/rise-of-red-skull.avif',
  GalaxysMostWanted: '/images/products/galaxys-most-wanted.webp',
  MadTitansShadow: '/images/products/mad-titans-shadow.webp',
  MutantGenesis: '/images/products/mutant-genesis.webp',
  SinisterMotives: '/images/products/sinister-motives.webp',
  AgeOfApocalypseCampaign: '/images/products/age-of-apocalypse.webp',
  AgentsOfShieldCampaign: '/images/products/agents-of-shield.webp',
  FearNoEvilCampaign: '/images/products/fear-no-evil.webp',
  ShadowlandCampaign: '/images/products/shadowland.webp',
  NextEvolution: '/images/products/next-evolution.webp',
}

const RULEBOOK_ROOT = '/rulebooks/'
export const campaignGuides: Record<CampaignKind, { namespace: string; overview: string[]; source: string }> = {
  RiseOfRedSkull: { namespace: '', overview: ["campaignMode", "newKeywords"], source: `${RULEBOOK_ROOT}mc10_the_rise_of_red_skull_rules_web.pdf` },
  GalaxysMostWanted: { namespace: 'galaxy', overview: ["campaignMode", "keywords", "market", "shipCommand"], source: `${RULEBOOK_ROOT}mc16_galaxys_most_wanted_rules_website-compressed.pdf` },
  MadTitansShadow: { namespace: 'mts', overview: ["campaignMode", "keywords"], source: `${RULEBOOK_ROOT}mc21_the_mad_titans_shadow_rulebook-compressed.pdf` },
  SinisterMotives: { namespace: 'sm', overview: ["campaignMode", "keywords", "reputationTrack"], source: `${RULEBOOK_ROOT}mc27_sinister_motives_rules_v5-compressed.pdf` },
  MutantGenesis: { namespace: 'mutantGenesis', overview: ["campaignMode", "roles", "futurePast", "keywords"], source: `${RULEBOOK_ROOT}mc32_mutant_genesis_rulebook_v5-compressed.pdf` },
  NextEvolution: { namespace: 'nextEvolution', overview: ["campaignMode", "campaignSchemes", "playerSideSchemes", "keywords"], source: `${RULEBOOK_ROOT}mc40_next_evolution_rulebook-web.pdf` },
  AgeOfApocalypseCampaign: { namespace: 'ageOfApocalypse', overview: ["campaignMode", "missionRules", "missionAttempts", "keywords"], source: `${RULEBOOK_ROOT}mc45_age_of_apocalypse_rulebook.pdf` },
  AgentsOfShieldCampaign: { namespace: 'aos', overview: ["campaignMode", "evidence", "board"], source: `${RULEBOOK_ROOT}mc50_rulebook-web.pdf` },
  FearNoEvilCampaign: { namespace: 'fearNoEvil', overview: ["intro", "campaignMode", "planning", "campaignSetup"], source: `${RULEBOOK_ROOT}mc60_rulebook-web.pdf` },
  ShadowlandCampaign: { namespace: 'shadowland', overview: ["campaignMode"], source: `${RULEBOOK_ROOT}MC65-Rulebook.pdf` },
}

// General rules remain available between games without revealing later scenario stories.
export const campaignGuideSections = (kind: CampaignKind, mode: CampaignMode) => {
  const guide = campaignGuides[kind]
  return [...guide.overview, ...(mode === 'ExpertCampaign' ? ['expertCampaign'] : [])]
    .map(section => `$story.${guide.namespace ? `${guide.namespace}.` : ''}${section}`)
}
