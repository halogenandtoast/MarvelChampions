import { expect, test } from '@playwright/test'

for (const activation of ['attack', 'scheme']) {
  test(`revealed cards stay separate and compact beside an ${activation}`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    await page.evaluate(async activation => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: River } = await load('/src/components/River.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const alone = ref(false)
      Object.assign(window, { showRevealedAlone: () => { alone.value = true } })
      const host = document.createElement('div')
      host.style.setProperty('--card-width', '100px')
      document.body.replaceChildren(host)
      createApp({ setup: () => () => h(River, {
        cards: alone.value ? [] : [{ id: 'boost', code: '01186', boost: true }],
        revealedCards: [{ id: 'revealed', code: '01187' }], elsewhere: new Set(),
        label: alone.value ? null : `Rhino ${activation}`,
        attacker: !alone.value && activation === 'attack' ? { id: 'enemy', code: '01094', name: 'Rhino', attack: 3 } : null,
        schemer: !alone.value && activation === 'scheme' ? { id: 'enemy', code: '01094', name: 'Rhino', scheme: 2 } : null,
        scheme: !alone.value && activation === 'scheme' ? { id: 'scheme', code: '01097', name: 'The Break-In!', from: 1, amount: 2, threshold: 7 } : null,
      }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    }, activation)
    const revealed = page.getByRole('region', { name: 'Revealed', exact: true })
    const active = page.locator(`.${activation}-lane`)
    await expect(revealed.locator('[data-card-id="revealed"]')).toBeVisible()
    await expect(revealed.locator('[data-card-id="enemy"], [data-card-id="boost"], [data-card-id="scheme"]')).toHaveCount(0)
    await expect(active.locator('[data-card-id="revealed"]')).toHaveCount(0)
    await expect(active.locator('[data-card-id="enemy"]')).toBeVisible()
    await expect(active.locator('[data-card-id="boost"]')).toBeVisible()
    expect((await revealed.boundingBox())!.width).toBeLessThan((await active.boundingBox())!.width)
    expect(await active.evaluate(el => getComputedStyle(el).getPropertyValue('--activation-color').trim()))
      .toBe(activation === 'attack' ? '#e0535b' : '#5d9be0')
    await page.evaluate(() => (window as any).showRevealedAlone())
    await expect(page.locator('.lane')).toHaveCount(1)
    await expect(revealed).not.toHaveClass(/compact/)
    expect((await revealed.boundingBox())!.width).toBeCloseTo((await page.locator('.river').boundingBox())!.width, 0)
  })
}

test('the event being resolved has its own lane until resolution finishes', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: River } = await load('/src/components/River.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const events = ref([{ id: 'played-event', code: '62023', name: 'Power Man and Iron Fist' }])
    Object.assign(window, { finishPlayedEvent: () => { events.value = [] } })
    const host = document.createElement('div')
    host.style.setProperty('--card-width', '100px')
    document.body.replaceChildren(host)
    createApp({ setup: () => () => h(River, {
      cards: [], resolvingEvents: events.value,
      revealedCards: [{ id: 'encounter', code: '01186' }], elsewhere: new Set(),
    }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  const resolving = page.getByRole('region', { name: 'Resolving', exact: true })
  await expect(resolving.locator('[data-card-id="played-event"]')).toBeVisible()
  await expect(resolving).toHaveClass(/compact/)
  await expect(page.getByRole('region', { name: 'Revealed', exact: true }).locator('[data-card-id="played-event"]')).toHaveCount(0)
  await expect(page.locator('.targets')).toHaveCount(0)
  await page.evaluate(() => (window as any).finishPlayedEvent())
  await expect(resolving).toHaveCount(0)
  await expect(page.getByRole('region', { name: 'Revealed', exact: true })).not.toHaveClass(/compact/)
})
