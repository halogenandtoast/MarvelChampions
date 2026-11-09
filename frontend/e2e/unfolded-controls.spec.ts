import { expect, test } from '@playwright/test'

for (const code of ['12001', '13001', '42001']) {
  test(`${code} keeps normal-sized controls when unfolded and exhausted`, async ({ page }) => {
    await page.goto(`/#/cards/${code}a`)
    await page.evaluate(async code => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: BoardCard } = await load('/src/components/BoardCard.vue')
      const { provideChoices } = await load('/src/composables/useChoices.ts')
      const { useCards } = await load('/src/cards.ts')
      await useCards().store.load()
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const card = reactive({ id: 'giant-controls', code: `${code}a`, faces: [`${code}a`, `${code}b`, `${code}c`], damage: 1, threat: 1, bombCounters: 1, counters: 2, stunCards: 2, stunned: true, confuseCards: 2, confused: true, exhausted: false })
      const host = document.createElement('div')
      host.style.cssText = '--card-width:120px; padding:180px;'
      document.body.replaceChildren(host)
      createApp({ setup() {
        provideChoices(() => ['changeForm', 'thwart', 'attack'].map((label, index) => ({ index, kind: 'ability', abilityType: 'Basic', label: `$label.${label}`, target: card.id })), () => {})
        return () => h(BoardCard, card)
      } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
      Object.assign(window, { giantControls: card })
    }, code)
    const card = page.locator('[data-card-id="giant-controls"]')
    const dimensions = () => card.evaluate(el => ({
      font: getComputedStyle(el.querySelector('.chip')!).fontSize,
      button: getComputedStyle(el.querySelector('.chip')!).width,
      tokens: [...el.querySelectorAll('.token img')].map(img => getComputedStyle(img).width),
      statuses: [...el.querySelectorAll('.statuses img')].map(img => getComputedStyle(img).width),
    }))
    await expect(card.locator('.chip')).toHaveCount(3)
    const normal = await dimensions()
    const expectNormalSizes = async () => {
      const current = await dimensions()
      expect(parseFloat(current.button)).toBeCloseTo(parseFloat(normal.button), 0)
      expect({ ...current, button: normal.button }).toEqual(normal)
    }
    await page.evaluate(code => { (window as any).giantControls.code = `${code}c` }, code)
    await expect(card).toHaveClass(/unfolded/)
    await page.waitForTimeout(450)
    await expectNormalSizes()
    const bounds = (await card.boundingBox())!
    const buttons = (await card.locator('.chips').boundingBox())!
    expect(buttons.x).toBeGreaterThanOrEqual(bounds.x)
    expect(buttons.x + buttons.width).toBeLessThanOrEqual(bounds.x + bounds.width)
    expect(buttons.y).toBeGreaterThan(bounds.y)
    expect(buttons.y + buttons.height).toBeLessThan(bounds.y + bounds.height)
    await page.evaluate(() => { (window as any).giantControls.exhausted = true })
    await page.waitForTimeout(300)
    await expectNormalSizes()
    const turned = (await card.boundingBox())!
    const turnedButtons = (await card.locator('.chips').boundingBox())!
    expect(turnedButtons.x).toBeGreaterThanOrEqual(turned.x)
    expect(turnedButtons.y).toBeGreaterThanOrEqual(turned.y)
    expect(turnedButtons.x + turnedButtons.width).toBeLessThanOrEqual(turned.x + turned.width)
    expect(turnedButtons.y + turnedButtons.height).toBeLessThanOrEqual(turned.y + turned.height)
  })
}
