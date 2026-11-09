import { expect, test } from '@playwright/test'

for (const width of [390, 1600]) {
  test(`set-aside cards can be browsed and update when moved at ${width}px`, async ({ page }) => {
    await page.setViewportSize({ width, height: 900 })
    await page.goto('/#/cards/38025')
    await page.evaluate(async () => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: VillainArea } = await load('/src/components/VillainArea.vue')
      const { default: CardOverlay } = await load('/src/components/CardOverlay.vue')
      const { useCards } = await load('/src/cards.ts')
      await useCards().store.load()
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const state = reactive({ cards: [
        { id: 'mystique', code: '38025', name: 'Mystique' },
        { id: 'misled-1', code: '38027', name: 'Misled' },
        { id: 'misled-2', code: '38027', name: 'Misled' },
      ] })
      const host = document.createElement('div')
      host.style.setProperty('--card-width', '108px')
      document.body.replaceChildren(host)
      createApp({ setup: () => () => [h(VillainArea, { game: {
        players: [], villains: [], minions: [], mainSchemes: [], sideSchemes: [],
        encounterDeckSize: 20, encounterDiscard: [], setAsideCards: state.cards,
      } }), h(CardOverlay)] }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
      Object.assign(window, { moveSetAsideCard: () => { state.cards = state.cards.filter((card: any) => card.id !== 'mystique') }, clearSetAside: () => { state.cards = [] } })
    })
    const pile = page.locator('[data-pile="set-aside"]')
    await expect(pile.locator('.count')).toHaveText('3')
    await pile.getByRole('button', { name: 'Set aside', exact: true }).click()
    const dialog = page.getByRole('dialog')
    await expect(dialog).toBeVisible()
    await expect(dialog.locator('[data-card-id="mystique"]')).toBeVisible()
    await expect(dialog.locator('[data-card-id^="misled-"]')).toHaveCount(2)
    await expect(dialog.locator('.chip')).toHaveCount(0)
    await page.evaluate(() => (window as any).moveSetAsideCard())
    await expect(dialog.locator('[data-card-id="mystique"]')).toHaveCount(0)
    await expect(pile.locator('.count')).toHaveText('2')
    await dialog.getByRole('button', { name: 'Close', exact: true }).click()
    await page.evaluate(() => (window as any).clearSetAside())
    await expect(pile).toHaveCount(0)
  })
}
