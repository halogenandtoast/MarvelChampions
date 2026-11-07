import { expect, test } from '@playwright/test'

test('dealt encounter cards highlight and submit the pending reveal choice', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: Pile } = await load('/src/components/Pile.vue')
    const source = await (await fetch('/src/components/Pile.vue')).text()
    const choicesPath = source.match(/from ["']([^"']*\/composables\/useChoices\.ts[^"']*)["']/)?.[1]
    if (!choicesPath) throw new Error('Pile choices import not found')
    const { provideChoices } = await load(choicesPath)
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const host = document.createElement('div')
    host.id = 'encounter-click-test'
    document.body.replaceChildren(host)
    createApp({ setup() {
      provideChoices(() => [{ index: 3, kind: 'target', target: 'dealt-player', label: '$label.revealDealtEncounterCard' }],
        (index: number) => { host.dataset.chosen = String(index) })
      return () => h(Pile, { label: 'Dealt encounters', pile: 'dealt-player', back: 'encounter', count: 20 })
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  const deck = page.getByRole('button', { name: 'Dealt encounters', exact: true })
  await expect(deck).toBeEnabled()
  await expect(page.locator('.pile')).toHaveClass(/active/)
  await deck.click()
  await expect(page.locator('#encounter-click-test')).toHaveAttribute('data-chosen', '3')
  await page.locator('#encounter-click-test').evaluate((host) => host.removeAttribute('data-chosen'))
  await deck.focus()
  await page.keyboard.press('Space')
  await expect(page.locator('#encounter-click-test')).toHaveAttribute('data-chosen', '3')
})
