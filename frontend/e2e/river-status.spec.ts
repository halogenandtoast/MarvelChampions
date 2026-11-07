import { expect, test } from '@playwright/test'

test('river announces activations without blocking clicks', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: River } = await load('/src/components/River.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const alert = ref('Venom Goblin attacks Daredevil')
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    createApp({ setup: () => () => h(River, { cards: [], elsewhere: new Set(), activationAlert: alert.value }) })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { riverAlert: alert })
  })
  const alert = page.getByRole('status')
  await expect(alert).toHaveText('Venom Goblin attacks Daredevil')
  expect(await alert.evaluate(el => getComputedStyle(el).pointerEvents)).toBe('none')
  for (const text of ['Venom Goblin schemes']) {
    await page.evaluate(text => { (window as any).riverAlert.value = text }, text)
    await expect(alert).toHaveText(text)
  }
})

for (const status of ['stunned', 'confused'] as const) {
  test(`river explains an event canceled by ${status}`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    await page.evaluate(async (status) => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: River } = await load('/src/components/River.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      document.body.replaceChildren(host)
      createApp({ setup: () => () => h(River, {
        cards: [], elsewhere: new Set(), shared: true,
        canceledEvent: { code: status === 'stunned' ? '01008' : '01060', statuses: [status] },
      }) })
        .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } }))
        .mount(host)
    }, status)
    const river = page.locator('.river')
    await expect(river).toContainText(status === 'stunned' ? 'Stunned: attack canceled' : 'Confused: thwart canceled')
    await expect(river.locator('.card')).toHaveCount(1)
    const image = river.locator('.canceled-status')
    await expect(image).toBeVisible()
    expect(await image.evaluate((img: HTMLImageElement) => img.complete && img.naturalWidth > 0)).toBe(true)
  })
}

for (const status of ['stunned', 'confused'] as const) {
  test(`river warns about ${status} while paying`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    await page.evaluate(async (status) => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: PayingTray } = await load('/src/components/PayingTray.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      host.style.setProperty('--card-width', '100px')
      document.body.replaceChildren(host)
      createApp({ setup: () => () => h(PayingTray, {
        elsewhere: new Set(), cost: {
          player: 'test', card: { id: 'event', code: '01060', name: 'For Justice!' },
          source: null, paid: [], owed: [null, null], anyAmount: false, spent: [], canceledBy: [status],
        },
      }) })
        .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } }))
        .mount(host)
    }, status)
    await expect(page.getByRole('status')).toContainText(status === 'stunned' ? 'Stunned will cancel this attack' : 'Confused will cancel this thwart')
    await expect(page.locator('.canceling-status')).toBeVisible()
  })
}
