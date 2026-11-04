import { expect, test } from '@playwright/test'
import { readFileSync } from 'node:fs'

const campaigns = [
  'RiseOfRedSkull', 'GalaxysMostWanted', 'MadTitansShadow', 'SinisterMotives',
  'MutantGenesis', 'NextEvolution', 'AgeOfApocalypseCampaign', 'AgentsOfShieldCampaign',
  'FearNoEvilCampaign', 'ShadowlandCampaign',
]

for (const kind of campaigns) {
  for (const mode of ['StandardCampaign', 'ExpertCampaign']) {
    test(`${kind} ${mode} has a complete readable guide`, async ({ page }) => {
      await page.setViewportSize({ width: 390, height: 667 })
      await page.goto('/#/cards/01001a')
      const expected = await page.evaluate(async ({ kind, mode }) => {
        const load = (path: string) => import(/* @vite-ignore */ path)
        const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
        const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
        const { default: StoryReading } = await load('/src/components/StoryReading.vue')
        const { campaignGuideSections, campaignGuides } = await load('/src/campaigns.ts')
        const { loadLocaleMessages } = await load('/src/locales/messages.ts')
        const { messages } = await loadLocaleMessages('en')
        const sections = campaignGuideSections(kind, mode)
        const source = campaignGuides[kind].source
        const host = document.createElement('div')
        document.body.replaceChildren(host)
        host.id = 'guide-host'
        createApp({ setup: () => () => h(StoryReading, {
          sections, sourceUrl: source, canContinue: false, reference: true,
          onClose: () => { host.dataset.closed = 'true' },
          onContinue: () => { host.dataset.advanced = 'true' },
        }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
        return { count: sections.length, source }
      }, { kind, mode })
      const dialog = page.getByRole('dialog')
      await expect(dialog).toBeVisible()
      // A missing/malformed locale section used to silently render an empty reading.
      await expect(dialog.locator('section')).toHaveCount(expected.count)
      for (const section of await dialog.locator('section').all()) {
        await expect(section.getByRole('heading')).not.toBeEmpty()
        expect(await section.locator('p, li').count()).toBeGreaterThan(0)
      }
      await expect(dialog.getByRole('link', { name: 'Official rulebook' })).toHaveAttribute('href', expected.source)
      await expect(dialog.getByRole('button', { name: 'Continue', exact: true })).toHaveCount(0)
      const footerClose = dialog.locator('footer').getByRole('button', { name: 'Close', exact: true })
      await expect(footerClose).toBeInViewport()
      const bounds = (await dialog.boundingBox())!
      expect(bounds.x).toBeGreaterThanOrEqual(0)
      expect(bounds.x + bounds.width).toBeLessThanOrEqual(390)
      expect(bounds.y + bounds.height).toBeLessThanOrEqual(667)
      await footerClose.click()
      await expect(page.locator('#guide-host')).toHaveAttribute('data-closed', 'true')
      await expect(page.locator('#guide-host')).not.toHaveAttribute('data-advanced')
    })
  }
}


test('every campaign story section has a title and readable content', () => {
  const story = JSON.parse(readFileSync(new URL('../src/locales/en/story.json', import.meta.url), 'utf8'))
  const visit = (value: unknown, path: string) => {
    expect(value, `${path} must be structured reading content`).not.toBeNull()
    expect(typeof value, `${path} must not be a raw string skipped by the reader`).toBe('object')
    expect(Array.isArray(value), path).toBe(false)
    const section = value as Record<string, unknown>
    if ('title' in section) {
      expect(typeof section.title, path).toBe('string')
      expect((section.title as string).trim().length, path).toBeGreaterThan(0)
      const lines = [...(section.text as unknown[] ?? []), ...(section.bullets as unknown[] ?? [])]
      expect(lines.length, `${path} must have paragraphs or rules`).toBeGreaterThan(0)
      for (const line of lines) {
        expect(typeof line, path).toBe('string')
        expect((line as string).trim().length, path).toBeGreaterThan(0)
      }
    } else {
      for (const [key, child] of Object.entries(section)) visit(child, `${path}.${key}`)
    }
  }
  visit(story, 'story')
})
