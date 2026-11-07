import { expect, test } from '@playwright/test'

test('the glider slides to its new scheme without shifting the cards', async ({ page }) => {
  await page.goto('/#/cards/27117a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: VillainArea } = await load('/src/components/VillainArea.vue')
    const { withViewTransition } = await load('/src/composables/useViewTransition.ts')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const game = reactive({
      villains: [], players: [], sideSchemes: [], environments: [], areas: [], encounterDeckSize: 20, encounterDiscard: [],
      mainSchemes: ['27117', '27118', '27119'].map((code, index) => ({
        id: code, code, name: ['Lower Manhattan', 'Midtown Manhattan', 'Upper Manhattan'][index],
        threat: 0, targetThreat: 10, glider: index === 1,
      })),
    })
    const host = document.createElement('div')
    host.style.setProperty('--card-width', '124px')
    document.body.replaceChildren(host)
    createApp({ setup: () => () => h(VillainArea, { game }) })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { moveGlider: () => {
      (window as any).gliderMove = withViewTransition(() => {
        game.mainSchemes.forEach((scheme: any, index: number) => { scheme.glider = index === 0 })
      })
    } })
  })
  const token = page.locator('.glider-marker')
  const before = (await token.boundingBox())!
  const tops = await page.locator('.scheme.main .card').evaluateAll(cards => cards.map(card => card.getBoundingClientRect().top))
  await page.evaluate(() => (window as any).moveGlider())
  await page.waitForFunction(() => document.getAnimations().some(animation => animation instanceof CSSAnimation && animation.animationName.includes('glider-counter')))
  await page.evaluate(() => (window as any).gliderMove)
  await expect(token).toHaveCount(1)
  expect((await token.boundingBox())!.x).toBeLessThan(before.x)
  expect(await page.locator('.scheme.main .card').evaluateAll(cards => cards.map(card => card.getBoundingClientRect().top))).toEqual(tops)
  await expect(page.locator('.scheme.main').first().locator('.glider-marker')).toBeVisible()
})
