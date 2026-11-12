import { expect, test } from '@playwright/test'

for (const width of [1500, 390]) {
  test(`Android Efficiency's physical payment choice renders its icon at ${width}px`, async ({ page }) => {
    await page.setViewportSize({ width, height: 1000 })
    await page.goto('/#/cards/01189')
    await page.evaluate(async () => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: QuestionZone } = await load('/src/components/QuestionZone.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      host.id = 'resource-question'
      document.body.replaceChildren(host)
      createApp({ setup: () => () => h(QuestionZone, {
        buttons: [
          { index: 0, kind: 'label', label: '$label.topCardAsDrone' },
          { index: 1, kind: 'label', label: 'Spend a {physical} resource' },
        ],
        onChoose: (index: number) => { host.dataset.chosen = String(index) },
      }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    })
    const spend = page.getByRole('button', { name: 'Spend a Physical resource' })
    await expect(spend.getByRole('img', { name: 'Physical' })).toHaveClass(/physical/)
    await expect(spend).not.toContainText('{physical}')
    await spend.click()
    await expect(page.locator('#resource-question')).toHaveAttribute('data-chosen', '1')
  })
}

test('resource tokens render icons while other braces and markup remain plain text', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: IconText } = await load('/src/components/IconText.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const host = document.createElement('div')
    host.id = 'icon-text'
    document.body.replaceChildren(host)
    createApp({ setup: () => () => h(IconText, { text: '{physical} {mental} {energy} {wild} {count} <b>plain</b>' }) })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  const text = page.locator('#icon-text')
  for (const name of ['Physical', 'Mental', 'Energy', 'Wild']) await expect(text.getByRole('img', { name, exact: true })).toBeVisible()
  await expect(text).toContainText('{count} <b>plain</b>')
  await expect(text.locator('b')).toHaveCount(0)
})
