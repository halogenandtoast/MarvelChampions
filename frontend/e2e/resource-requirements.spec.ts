import { expect, test } from '@playwright/test'

test('payment explains unmet resource type requirements instead of saying covered', async ({ page }) => {
  await page.goto('/#/cards/47023')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: PayingTray } = await load('/src/components/PayingTray.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const cost = reactive({ player: 'payer', card: null, source: null, paid: ['Physical', 'Mental'], owed: [], anyAmount: false, spent: [], resourceRequirements: [{ kind: 'same', count: 2, met: false }] })
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    createApp({ setup: () => () => h(PayingTray, { cost, elsewhere: new Set() }) })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { constrainedPayment: cost })
  })
  const tray = page.locator('.paying')
  await expect(tray).toContainText('Requires 2 resources of the same type')
  await expect(tray).not.toContainText('Cost covered')
  await expect(tray).not.toContainText('Needs 0 more')
  await expect(tray).not.toHaveClass(/covered/)
  await page.evaluate(() => {
    const cost = (window as any).constrainedPayment
    cost.paid = ['Physical', 'Physical']
    cost.resourceRequirements[0].met = true
  })
  await expect(tray).toContainText('Cost covered')
  await expect(tray).toHaveClass(/covered/)
  await expect(tray).not.toContainText('Requires')
  await page.evaluate(() => {
    const cost = (window as any).constrainedPayment
    cost.resourceRequirements[0] = { kind: 'different', count: 2, met: false }
  })
  await expect(tray).toContainText('Requires 2 different resource types')
  await expect(tray).not.toHaveClass(/covered/)
})
