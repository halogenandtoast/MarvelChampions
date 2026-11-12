import { expect, test } from '@playwright/test'

for (const width of [1500, 390]) {
  test(`a defeated-ally response names Maria Hill at ${width}px`, async ({ page }) => {
    await page.setViewportSize({ width, height: 1000 })
    await page.goto('/#/cards/01067')
    await page.evaluate(async () => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: QuestionZone } = await load('/src/components/QuestionZone.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      host.id = 'response-question'
      document.body.replaceChildren(host)
      createApp({ setup: () => () => h(QuestionZone, {
        prompt: '$label.respondToDefeatedAlly ally=s:"Maria Hill"',
        buttons: [{ index: 0, kind: 'label', label: '$label.noResponse' }],
        onChoose: (index: number) => { host.dataset.chosen = String(index) },
      }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    })
    await expect(page.getByRole('heading', { name: "Respond to Maria Hill's defeat" })).toBeVisible()
    await page.locator('.options button').click()
    await expect(page.locator('#response-question')).toHaveAttribute('data-chosen', '0')
  })
}
