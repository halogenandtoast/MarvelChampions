import { expect, test } from '@playwright/test'

test('the active banner moves in the villain card snapshot', async ({ page }) => {
  await page.goto('/#/cards/01113')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: VillainArea } = await load('/src/components/VillainArea.vue')
    const { withViewTransition } = await load('/src/composables/useViewTransition.ts')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const game = reactive({
      villains: [
        { id: 'active-villain', code: '01113', name: 'Rhino', active: true, hitPoints: 14, damage: 0, scheme: 1, attack: 2 },
        { id: 'other-villain', code: '01116', name: 'Klaw', active: false, hitPoints: 12, damage: 0, scheme: 2, attack: 0 },
      ],
      players: [], sideSchemes: [], mainSchemes: [], environments: [], areas: [], encounterDeckSize: 20, encounterDiscard: [],
    })
    const host = document.createElement('div')
    host.style.setProperty('--card-width', '124px')
    document.body.replaceChildren(host)
    createApp({ setup: () => () => h(VillainArea, { game }) })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { moveVillains: () => {
      (window as any).villainMove = withViewTransition(() => { game.villains.reverse() })
    } })
  })
  const banner = page.locator('.active-flag')
  await expect(banner).toBeVisible()
  expect(await banner.evaluate(el => {
    for (let parent = el.parentElement; parent; parent = parent.parentElement) {
      const name = getComputedStyle(parent).viewTransitionName
      if (name && name !== 'none') return name
    }
    return null
  })).toBe('cactive-villain')
  const before = (await banner.boundingBox())!
  await page.evaluate(() => (window as any).moveVillains())
  await page.waitForFunction(() => document.getAnimations().some(animation =>
    animation instanceof CSSAnimation && animation.animationName.includes('cactive-villain')))
  await page.evaluate(() => (window as any).villainMove)
  await expect(page.locator('[data-card-id="active-villain"] .active-flag')).toBeVisible()
  expect((await banner.boundingBox())!.x).toBeGreaterThan(before.x)
})
