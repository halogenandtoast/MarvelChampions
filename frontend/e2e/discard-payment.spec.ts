import { expect, test } from '@playwright/test'

test('discard-cost payments show the printed cost still required', async ({ page }) => {
  await page.goto('/#/cards/27136')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: PayingTray } = await load('/src/components/PayingTray.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const cost = reactive({ player: 'player', card: null, source: 'glider', paid: [], owed: [], anyAmount: false, spent: [], discardedCost: { paid: 0, required: 3 } })
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    createApp({ setup: () => () => h(PayingTray, {
      cost, elsewhere: new Set(), things: { glider: { code: '27136', name: 'Advanced Glider' } },
    }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { discardPayment: cost })
  })
  const tray = page.locator('.paying')
  await expect(tray).toContainText('0 / 3')
  await expect(tray).toContainText('Discard cards costing 3 more')
  await expect(tray).not.toHaveClass(/covered/)
  await page.evaluate(() => { (window as any).discardPayment.discardedCost.paid = 2 })
  await expect(tray).toContainText('2 / 3')
  await expect(tray).toContainText('Discard cards costing 1 more')
  await expect(tray).not.toHaveClass(/covered/)
  await page.evaluate(() => { (window as any).discardPayment.discardedCost.paid = 4 })
  await expect(tray).toContainText('Cost covered')
  await expect(tray).toHaveClass(/covered/)
})
